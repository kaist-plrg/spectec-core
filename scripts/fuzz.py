import os
import subprocess
import shutil
import argparse
import time
import copy
import multiprocessing
import psutil
import sys
import signal
from typedefs import *
from typing import Dict, List, Tuple, Union, Optional, Callable
from coverage_utils import (
    read_coverage,
    write_coverage,
    union_coverage,
    read_reductions,
    write_reductions,
    log,
)
from reduce import reduce_program, reduce_from_coverage, reduce_likely_hits, set_pgid
from cleanup import run_coverage, reduce_final

from dataclasses import dataclass


@dataclass
class FuzzConfig:
    work_dir: Directory
    loops: int
    spec: Directory
    include: Directory
    exclude: Directory
    ignores: List[Filepath]
    coverage: Filepath
    mode: str
    reduce: bool
    timeout: int
    creduce_configs: CReduceConfigs


def parse_args() -> FuzzConfig:
    parser = argparse.ArgumentParser(description="Fuzzer.")

    parser.add_argument("dir", type=str, help="Path to the working directory")
    parser.add_argument("--loops", type=int, default=2, help="Fuzz loop count")
    parser.add_argument("--spec", type=str, default="spec")
    parser.add_argument("--include", type=str, default="p4c/p4include")
    parser.add_argument("--exclude", type=str, default="excludes")
    parser.add_argument(
        "--ignores",
        nargs="*",
        type=str,
        default=["coverage/relation.ignore", "coverage/function.ignore"],
    )
    parser.add_argument("--coverage", type=str, default="coverage/p4c-pos.coverage")
    parser.add_argument("--reduce", action="store_true")
    parser.add_argument("--timeout", type=int, default=12*60*60, help="Fuzzer timeout in seconds.")
    parser.add_argument("--cores", type=int)
    parser.add_argument("--timeout-interesting", type=int, default=10)
    parser.add_argument("--timeout-creduce", type=int, default=25)
    parser.add_argument("--mode", choices=["random", "derive", "hybrid"], required=True)

    args = parser.parse_args()

    # P4CHERRY_PATH must be set
    if os.getenv("P4CHERRY_PATH") is None:
        print("Error: P4CHERRY_PATH environment variable is not set.")
        exit(1)
    P4SPECTEC_DIR: Directory = Directory(str(os.getenv("P4CHERRY_PATH")))
    print(f"[CONFIG] P4CHERRY_PATH: {P4SPECTEC_DIR}")

    return FuzzConfig(
        work_dir=args.dir,
        loops=args.loops,
        spec=args.spec,
        include=args.include,
        exclude=args.exclude,
        ignores=args.ignores,
        coverage=args.coverage,
        mode=args.mode,
        reduce=args.reduce,
        timeout=args.timeout,
        creduce_configs=CReduceConfigs(
            p4spectec_dir=P4SPECTEC_DIR,
            cores=args.cores,
            timeout_interesting=args.timeout_interesting,
            timeout_creduce=args.timeout_creduce,
        ),
    )


def fuzzing_campaign(config: FuzzConfig) -> None:

    os.setsid()  # only in main process, before any subprocesses are started
    pgid = os.getpid()

    #
    # Configuration check
    #

    # Configure working directory

    WORK_DIR: Directory = Directory(config.work_dir)
    if os.path.exists(WORK_DIR):
        print(f"Error: {WORK_DIR} already exists.")
        exit(1)
    os.makedirs(WORK_DIR)
    print(f"[CONFIG] Working directory: {WORK_DIR}")

    LOOPS: int = config.loops
    if LOOPS < 1:
        print("Error: Loops must be greater than 0.")
        exit(1)
    print(f"[CONFIG] Loop count: {LOOPS}")

    # Configure SpecTec

    SPEC_DIR: Directory = Directory(config.spec)
    if not os.path.isdir(SPEC_DIR):
        print(f"Error: Spec directory {SPEC_DIR} does not exist.")
        exit(1)
    SPEC_FILES: List[Filepath] = [
        Filepath(os.path.join(SPEC_DIR, file))
        for file in os.listdir(SPEC_DIR)
        if file.endswith(".watsup")
    ]
    SPEC_FILES = sorted(SPEC_FILES)
    print(f"[CONFIG] Spec files: {SPEC_FILES}")

    INCLUDE_DIR: Directory = Directory(config.include)
    if not os.path.isdir(INCLUDE_DIR):
        print(f"Error: Include directory {INCLUDE_DIR} does not exist.")
        exit(1)
    print(f"[CONFIG] Include directory: {INCLUDE_DIR}")

    EXCLUDE_DIR: Directory = Directory(config.exclude)
    if not os.path.isdir(EXCLUDE_DIR):
        print(f"Error: Exclude directory {EXCLUDE_DIR} does not exist.")
        exit(1)
    print(f"[CONFIG] Exclude directory: {EXCLUDE_DIR}")

    IGNORE_FILES: List[Filepath] = config.ignores
    COVERAGE_FILE: Filepath = Filepath(config.coverage)
    if not os.path.isfile(COVERAGE_FILE):
        print(f"Error: Coverage file {COVERAGE_FILE} does not exist.")
        exit(1)
    print(f"[CONFIG] Coverage file: {COVERAGE_FILE}")

    # Configuring C-Reduce

    C_REDUCE_CONFIGS: CReduceConfigs = config.creduce_configs
    loop_idx: int = 0

    # Template command for SpecTec,
    # goes without fuel, coverage file, and campaign name

    spectec_command_template = [
        "./p4spectec",
        "testgen",
        *SPEC_FILES,
        "-silent",
        "-i",
        INCLUDE_DIR,
        "-e",
        EXCLUDE_DIR,
        *[cmd for file in IGNORE_FILES for cmd in ("-ignore", file)],
        "-gen",
        WORK_DIR,
    ]
    if config.mode == "random":
        spectec_command_template += ["-random"]
    elif config.mode == "hybrid":
        spectec_command_template += ["-hybrid"]

    # Fuzzing campaign data structures

    total_coverage: Coverage = read_coverage(COVERAGE_FILE)
    fuzzer_coverage: Coverage = {}
    reductions: Reductions = {}

    # Fuzzing campaign hyperparameters

    MAX_REDUCTIONS_PER_PID: int = 0
    REDUCE = config.reduce

    #
    # Partition the coverage into Close-misses(to reduce) and others
    #

    fuzzer_coverage = copy.deepcopy(total_coverage)
    for origin in total_coverage:
        for pid, (status, filenames) in total_coverage[origin].items():
            if status == Status.CLOSE_MISS:
                fuzzer_coverage[origin][pid] = (status, [])

    #
    # In the following loops,
    #
    #
    # 1) REDUCE: reducer takes the {total coverage} and reduces the smallest file for each close-missed phantom id,
    #   writes: a {reductions file} containing the list of reductions done in this reduction.
    #   mutates: the {total coverage} with the filenames updated to their reduced versions.
    #   1-1) the reducer substitutes the original file for a target pid to the file newly reduced w.r.t the pid
    #   1-2) Close-miss programs in fuzzer coverage is cleared, and reduced files are added to their pids.
    #
    # 2) FUZZ: fuzzer takes {a fuzzer coverage} which holds the programs targeted for mutation (reduced programs only).
    #   and only mutates the programs in this list, writing {a fuzzer coverage} with any new findings through mutation.
    #   2-1) the {total coverage} is updated with the new findings with `union`.

    reduced_files_dir: Directory = Directory(os.path.join(WORK_DIR, "reduced"))
    while loop_idx < LOOPS:
        print(f"\n[INFO] === Starting loop{loop_idx} ===")

        if REDUCE:
            # === 1) REDUCE ===

            name_reduce_campaign: str = f"reduce{loop_idx}"
            reduce_dir: Directory = Directory(
                os.path.join(WORK_DIR, name_reduce_campaign)
            )
            os.makedirs(reduce_dir, exist_ok=True)

            reductions = {}
            print(f"\n[INFO] === Starting reduce{loop_idx} ===")
            reduce_from_coverage(
                total_coverage,
                fuzzer_coverage,
                MAX_REDUCTIONS_PER_PID,
                reductions,
                reduce_dir,
                reduced_files_dir,
                C_REDUCE_CONFIGS,
                pgid,
            )

            print(
                f"\n[INFO] === Finished reduce{loop_idx} with {len(reductions)} reductions ==="
            )
            # Log reductions
            reductions_file: Filepath = Filepath(
                os.path.join(reduce_dir, "reduced.reductions")
            )
            write_reductions(reductions_file, reductions)

            # Write the resulting fuzzer coverage
            fuzzer_coverage_file: Filepath = Filepath(
                os.path.join(reduce_dir, "fuzz.coverage")
            )
            print(
                f"\n[INFO] === Writing fuzzer coverage into {fuzzer_coverage_file} ==="
            )
            write_coverage(fuzzer_coverage_file, fuzzer_coverage)

        elif loop_idx == 0:
            fuzzer_coverage_file = COVERAGE_FILE

        else:
            fuzzer_coverage_file = Filepath(
                os.path.join(WORK_DIR, f"fuzz{loop_idx-1}", "final.coverage")
            )

        # === 2) FUZZ ===

        name_fuzz_campaign = f"fuzz{loop_idx}"
        fuzz_dir: Directory = Directory(os.path.join(WORK_DIR, name_fuzz_campaign))
        print(f"\n[INFO] === Fuzzing in {fuzz_dir} ===")

        spectec_fuzz_command = spectec_command_template.copy() + [
            "-seed",
            str(loop_idx),
            "-fuel",
            "10",
            "-warm",
            fuzzer_coverage_file,
            "-name",
            name_fuzz_campaign,
        ]

        print(f"\n[INFO] === Starting fuzz{loop_idx} ===")
        process = subprocess.Popen(
            spectec_fuzz_command,
            preexec_fn=set_pgid(pgid),
        )
        result = process.wait()
        os.makedirs(fuzz_dir, exist_ok=True)

        # 2-1) Fuzzer takes a fuzzer coverage and returns a final coverage
        final_coverage_file: Filepath = Filepath(
            os.path.join(fuzz_dir, "final.coverage")
        )
        print(f"\n[INFO] === Reading fuzzer coverage from {final_coverage_file} ===")
        final_coverage = read_coverage(final_coverage_file)

        # 2-2) Total coverage is updated with the findings from final coverage
        total_coverage = union_coverage(total_coverage, final_coverage)
        total_coverage_file: Filepath = Filepath(
            os.path.join(fuzz_dir, "total.coverage")
        )
        print(f"\n[INFO] === Writing merged coverage into {total_coverage_file} ===")
        write_coverage(total_coverage_file, total_coverage)

        # 2-3) Hits in total coverage are promoted to fuzzer coverage
        for origin in total_coverage:
            for pid, (status, filenames) in total_coverage[origin].items():
                if status == Status.HIT_LIKELY or status == Status.HIT_UNLIKELY:
                    fuzzer_coverage[origin][pid] = (status, filenames)

        loop_idx += 1


def terminate_process_tree(process):
    try:
        try:
            parent = psutil.Process(process.pid)
            children = parent.children(recursive=True)
        except psutil.NoSuchProcess:
            print(f"Process {process.pid} already exited.")
            return

        for child in children:
            try:
                print(f"Killing subprocess {child.pid}")
                child.kill()
            except psutil.NoSuchProcess:
                print(f"Subprocess {child.pid} already exited.")
        try:
            print(f"Killing parent process {parent.pid}")
            parent.kill()
        except psutil.NoSuchProcess:
            print(f"Parent process {parent.pid} already exited.")
    except Exception as e:
        print(f"Error while terminating process tree: {e}")


def signal_handler(sig, frame):
    print("\n[INFO] Caught Ctrl+C or termination signal, cleaning up...")
    if p.is_alive():
        terminate_process_tree(p)
        p.join()
    print("[INFO] All processes terminated.")
    sys.exit(1)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal_handler)  # Handles Ctrl+C
    signal.signal(signal.SIGTERM, signal_handler)  # Handles kill command


    config = parse_args()
    p = multiprocessing.Process(target=fuzzing_campaign, args=(config,))
    p.start()
    p.join(config.timeout)

    if p.is_alive():
        print(f"\n[ERROR] Timeout after {config.timeout} seconds. Killing process tree.")
        terminate_process_tree(p)
        p.join()

    else:
        print("\n[INFO] Script finished within timeout.")
    TESTDATA_DIR: Directory = Directory("p4c/testdata/p4_16_samples")
    OUTPUT_PATH: Filepath = Filepath(os.path.join(config.work_dir, "total.coverage"))
    run_coverage(
        config.work_dir,
        config.spec,
        config.include,
        config.exclude,
        config.ignores,
        TESTDATA_DIR,
        OUTPUT_PATH,
    )
    total_coverage: Coverage = read_coverage(OUTPUT_PATH)
    reduce_final(config.work_dir, total_coverage, config.creduce_configs)
