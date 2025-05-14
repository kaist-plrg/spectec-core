import os
import subprocess
import shutil
import argparse
import time
from typing import Dict, List, Tuple, Union, Optional
from coverage_utils import (
    read_coverage,
    write_coverage,
    write_reductions,
    read_reductions,
    Status,
    log,
    PID,
    Origin,
    Basename,
    CoverageEntry,
    Coverage,
    Directory,
    Filepath,
    Reductions,
)
from reduce import reduce_program


#
# Parse command-line arguments
#
parser = argparse.ArgumentParser(description="Fuzzer.")

# Arguments for the campaign
parser.add_argument("dir", type=str, help="Path to the working directory")
parser.add_argument("--loops", type=int, default=2, help="Fuzz loop count")

# Arguments for SpecTec
parser.add_argument(
    "--spec", type=str, default="spec", help="Spec directory for SpecTec"
)
parser.add_argument(
    "--include",
    type=str,
    default="p4/testdata/arch",
    help="Include directory for SpecTec",
)
parser.add_argument(
    "--ignores",
    nargs="*",
    type=str,
    default=["coverage/relation.ignore", "coverage/function.ignore"],
    help="List of ignore files for skipping phantom ids",
)
parser.add_argument(
    "--coverage",
    type=str,
    default="coverage/p4c-all.coverage",
    help="Path to initial coverage data",
)

# Arguments for C-Reduce
parser.add_argument(
    "--cores",
    type=int,
    help="Number of cores for creduce (default: creduce finds an optimal setting)",
)
parser.add_argument(
    "--timeout",
    type=int,
    default=10,
    help="Timeout in seconds for interestingness test (default: 10 seconds)",
)
parser.add_argument(
    "--timeout-creduce",
    type=int,
    default=25,
    help="Timeout in seconds for creduce (default: 25 seconds)",
)

args, unknown_args = parser.parse_known_args()

#
# Configuration check
#
# Configure working directory
WORK_DIR: Directory = Directory(args.dir)
if os.path.exists(WORK_DIR):
    print(f"Error: {WORK_DIR} already exists.")
    exit(1)
os.makedirs(WORK_DIR)
print(f"[CONFIG] Working directory: {WORK_DIR}")

LOOPS: int = args.loops
if LOOPS < 1:
    print("Error: Loops must be greater than 0.")
    exit(1)
print(f"[CONFIG] Loop count: {LOOPS}")

# Configure SpecTec
SPEC_DIR: str = args.spec
if not os.path.isdir(SPEC_DIR):
    print(f"Error: Spec directory {SPEC_DIR} does not exist.")
    exit(1)
SPEC_FILES: List[str] = [
    os.path.join(SPEC_DIR, file)
    for file in os.listdir(SPEC_DIR)
    if file.endswith(".watsup")
]
SPEC_FILES = sorted(SPEC_FILES)
print(f"[CONFIG] Spec files: {SPEC_FILES}")

INCLUDE_DIR: str = args.include
if not os.path.isdir(INCLUDE_DIR):
    print(f"Error: Include directory {INCLUDE_DIR} does not exist.")
    exit(1)
print(f"[CONFIG] Include directory: {INCLUDE_DIR}")

IGNORE_FILES: List[str] = args.ignores
COVERAGE_FILE: str = args.coverage
if not os.path.isfile(COVERAGE_FILE):
    print(f"Error: Coverage file {COVERAGE_FILE} does not exist.")
    exit(1)
print(f"[CONFIG] Coverage file: {COVERAGE_FILE}")

# Configuring C-Reduce

# P4CHERRY_PATH must be set
if os.getenv("P4CHERRY_PATH") is None:
    print("Error: P4CHERRY_PATH environment variable is not set.")
    exit(1)
P4SPECTEC_DIR: Directory = Directory(str(os.getenv("P4CHERRY_PATH")))
print(f"[CONFIG] P4CHERRY_PATH: {P4SPECTEC_DIR}")

CORES: Union[int, None] = args.cores

#
# Main fuzzing loop
#

loop_idx: int = 0

# Template command for SpecTec,
# goes without fuel, coverage file, and campaign name
spectec_command_template = [
    "./p4spectec",
    "testgen",
    *SPEC_FILES,
    "-silent",
    "-hybrid",
    "-i",
    INCLUDE_DIR,
    *[cmd for file in IGNORE_FILES for cmd in ("-ignore", file)],
    "-gen",
    WORK_DIR,
]

#
# Initial fuzzing iteration
#

name_fuzz_campaign = f"fuzz{loop_idx}"

print(f"\n[DEBUG] === Starting {name_fuzz_campaign} ===")
spectec_init_command = spectec_command_template.copy() + [
    "-seed",
    str(loop_idx),
    "-fuel",
    "1",
    "-warm",
    COVERAGE_FILE,
    "-name",
    name_fuzz_campaign,
]

result = subprocess.run(spectec_init_command, check=True)

#
# Fuzzing loop: reduce and generate
#

total_coverage: Coverage = {}
fuzzer_coverage: Coverage = {}
reductions: Reductions = {}
MAX_REDUCTIONS: int = 0
TARGETED = True
REDUCE = True
RESUMED: bool = False


while loop_idx < LOOPS:
    print(f"\n[DEBUG] === Starting loop {loop_idx} ===")

    # Get and read the fuzzer output coverage file
    name_fuzz_campaign = f"fuzz{loop_idx}"
    prev_coverage_file: Filepath = Filepath(
        os.path.join(WORK_DIR, name_fuzz_campaign, "final.coverage")
    )
    fuzzer_coverage = read_coverage(prev_coverage_file)
    
    reducer_coverage: Coverage = {}
    # partition the coverage into Close-misses and others
    for origin in fuzzer_coverage:
        for pid, (status, filenames) in fuzzer_coverage[origin].items():
            if status == Status.CLOSE_MISS:
                if origin not in reducer_coverage:
                    reducer_coverage[origin] = {}
                reducer_coverage[origin][pid] = (status, filenames)
                fuzzer_coverage[origin][pid] = (status, {})


    # Setup the directory for reduction
    name_reduce_campaign: str = f"reduce{loop_idx}"
    reduce_dir: Directory = Directory(os.path.join(WORK_DIR, name_reduce_campaign))
    os.makedirs(reduce_dir, exist_ok=True)

    # Source from reduction output if resumed (legacy)
    if RESUMED and loop_idx > 0:
        prev_reductions_file: Filepath = Filepath(
            os.path.join(WORK_DIR, f"reduce{loop_idx-1}", "reduced.reductions")
        )
        reductions = read_reductions(prev_reductions_file)

    global_log_path: Filepath = Filepath(os.path.join(reduce_dir, "reducer.log"))
    with open(global_log_path, "a") as global_log_file:
        # Reduce the smallest file for each close-missed phantom id
        for origin in reducer_coverage:
            for pid, (status, filenames) in reducer_coverage[origin].items():

                if MAX_REDUCTIONS > 0 and pid in reductions and len(reductions[pid]) >= MAX_REDUCTIONS:
                    log(f"Skipping: Already reduced {MAX_REDUCTIONS} files for pid={pid}", global_log_file)
                    continue

                # Check if the filenames are valid
                filenames_valid: List[Filepath] = [
                    Filepath(f) for f in filenames if os.path.isfile(f)
                ]
                # Find the smallest unreduced file and reduce it
                filenames_unreduced: List[Filepath] = [
                    f
                    for f in filenames_valid
                    if not os.path.basename(f).startswith("r_")
                ]

                if not filenames_unreduced:
                    log(
                        f"Skipping: No unreduced files found for pid={pid}",
                        global_log_file,
                    )
                    continue
                smallest_file: Filepath = min(filenames_unreduced, key=os.path.getsize)
                reducer_result: Optional[Filepath] = reduce_program(
                    reduce_dir,
                    pid,
                    smallest_file,
                    P4SPECTEC_DIR,
                    CORES,
                    args.timeout,
                    args.timeout_creduce,
                )
                if reduced_file is None:
                    log(
                        f"Failed to reduce {smallest_file} for pid={pid}",
                        global_log_file,
                    )
                    continue
                reduced_file: Filepath = reducer_result[0]

                # update total coverage: substitute original with reduced file
                total_coverage[origin][pid][1].remove(smallest_file)
                total_coverage[origin][pid][1].append(reduced_file)


                # if TARGETED, append the new file to just the target PID
                if TARGETED:
                    fuzzer_coverage[origin][pid][1].append(reduced_file)
                    #
                # if not TARGETED, append the new file to all PIDs
                else:
                    for origin in fuzzer_coverage:
                        for pid, (status, filenames) in fuzzer_coverage[origin].items():
                            if status == Status.CLOSE_MISS:
                                fuzzer_coverage[origin][pid][1].append(reduced_file)

                # update reductions
                reductions.setdefault(pid, []).append(reduced_file)

        # Output the reduced files as a coverage file
        fuzzer_coverage_file: Filepath = Filepath(
            os.path.join(reduce_dir, "target.coverage")
        )
        write_coverage(reduced_coverage_file, total_coverage)

        # log reductions
        reductions_file: Filepath = Filepath(
            os.path.join(reduce_dir, "reduced.reductions")
        )
        write_reductions(reductions_file, reductions)

        # Fuzzing with the reduced files
        loop_idx += 1

        spectec_fuzz_command = spectec_command_template.copy() + [
            "-seed",
            str(loop_idx),
            "-fuel",
            "1",
            "-warm",
            coverage_file,
            # "-reductions",
            # reductions_file,
            "-name",
            name_fuzz_campaign,
        ]

        result = subprocess.run(spectec_fuzz_command, check=True)

#
# End of the fuzzing loop
#

# TODO: From the final coverage, reduce likely hits
