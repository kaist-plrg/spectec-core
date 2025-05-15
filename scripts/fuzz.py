import os
import subprocess
import shutil
import argparse
import time
import copy
from typedefs import *
from typing import Dict, List, Tuple, Union, Optional
from coverage_utils import (
    read_coverage,
    write_coverage,
    union_coverage,
    read_reductions,
    write_reductions,
    log,
)
from reduce import reduce_program, reduce_from_coverage


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


C_REDUCE_CONFIGS: CReduceConfigs = {
    "p4spectec_dir": P4SPECTEC_DIR,
    "cores": CORES,
    "timeout_interesting": args.timeout,
    "timeout_creduce": args.timeout_creduce,
}
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
print(f"\n[DEBUG] === Starting {name_fuzz_campaign} ===")
result = subprocess.run(spectec_init_command, check=True)

#
# Fuzzing loop: reduce and generate
#


# Initialize based on fuzz0

total_coverage: Coverage = {}
fuzzer_coverage: Coverage = {}
reductions: Reductions = {}
MAX_REDUCTIONS_PER_PID: int = 0
TARGETED = False

#
# From the initial fuzz cycle, we generate a good seed pool,
# and take the resulting coverage as the initial total coverage.
#

initial_coverage_file: Filepath = Filepath(
    os.path.join(WORK_DIR, "fuzz0", "final.coverage")
)
total_coverage = read_coverage(initial_coverage_file)

#
# partition the coverage into Close-misses(to reduce) and others
#
fuzzer_coverage = copy.deepcopy(total_coverage)
for origin in total_coverage:
    for pid, (status, filenames) in total_coverage[origin].items():
        if status == Status.CLOSE_MISS:
            fuzzer_coverage[origin][pid] = (status, [])

#
# In the following loops,
#
# 1) the fuzzer takes {a fuzzer coverage} which holds the programs targeted for mutation (reduced programs only).
#   and only mutates the programs in this list, returning {a fuzzer coverage} with any new findings through mutation.
#   1-1) the total coverage is updated with the new findings with `union`.
#
# 2) the reducer takes {a total coverage} and reduces the smallest file for each close-missed phantom id,
#   returning {a reductions file} containing the accumulated list of reductions per pid.
#   2-1) the reducer substitutes the original file for a target pid to the file newly reduced w.r.p the pid
#   2-2) TARGETED: Close-miss programs in fuzzer coverage is cleared, and reduced files are added to their pids.
#   2-3) NON-TARGETED: Close-miss programs in fuzzer coverage is cleared.
#       All reduced programs are treated as a new seed, and seed coverage is computed.
#       This is then merged with the fuzzer coverage.
#

reduced_files_dir: Directory = Directory(os.path.join(WORK_DIR, "reduced"))
while loop_idx < LOOPS:
    print(f"\n[DEBUG] === Starting loop{loop_idx} ===")

    name_reduce_campaign: str = f"reduce{loop_idx}"
    reduce_dir: Directory = Directory(os.path.join(WORK_DIR, name_reduce_campaign))
    os.makedirs(reduce_dir, exist_ok=True)

    print(f"\n[DEBUG] === Starting reduce{loop_idx} ===")
    # run reducer
    reduce_from_coverage(
        total_coverage,
        fuzzer_coverage,
        MAX_REDUCTIONS_PER_PID,
        reductions,
        reduce_dir,
        reduced_files_dir,
        C_REDUCE_CONFIGS,
        TARGETED,
    )

    print(
        f"\n[DEBUG] === Finished reduce{loop_idx} with {len(reductions)} reductions ==="
    )
    # log reductions
    reductions_file: Filepath = Filepath(os.path.join(reduce_dir, "reduced.reductions"))
    write_reductions(reductions_file, reductions)

    # for non-target mode, measure coverage of reduced seeds
    if not TARGETED:
        # Output the reduced files as a coverage file
        reduced_coverage_file: Filepath = Filepath(
            os.path.join(reduce_dir, "reduced.coverage")
        )
        spectec_coverage_command = [
            "./p4spectec",
            "cover-sl",
            *SPEC_FILES,
            "-i",
            INCLUDE_DIR,
            *[cmd for file in IGNORE_FILES for cmd in ("-ignore", file)],
            "-d",
            reduced_files_dir,
            "-cov",
            reduced_coverage_file,
        ]

        print(f"\n[DEBUG] === Measuring reduced coverage for reduce{loop_idx} ===")
        result = subprocess.run(spectec_coverage_command, check=True)
        reduced_coverage: Coverage = read_coverage(reduced_coverage_file)
        # fuzzer coverage is unified with reduced coverage
        fuzzer_coverage = union_coverage(fuzzer_coverage, reduced_coverage)

    # Output the reduced files as a coverage file
    fuzzer_coverage_file: Filepath = Filepath(os.path.join(reduce_dir, "fuzz.coverage"))
    print(f"\n[DEBUG] === Writing fuzzer coverage into {fuzzer_coverage_file} ===")
    write_coverage(fuzzer_coverage_file, fuzzer_coverage)

    # Fuzzing with the reduced files
    loop_idx += 1
    name_fuzz_campaign = f"fuzz{loop_idx}"
    fuzz_dir: Directory = Directory(os.path.join(WORK_DIR, name_fuzz_campaign))
    print(f"\n[DEBUG] === Fuzzing in {fuzz_dir} ===")

    spectec_fuzz_command = spectec_command_template.copy() + [
        "-seed",
        str(loop_idx),
        "-fuel",
        "1",
        "-warm",
        fuzzer_coverage_file,
        "-name",
        name_fuzz_campaign,
    ]

    print(f"\n[DEBUG] === Starting fuzz{loop_idx} ===")
    result = subprocess.run(spectec_fuzz_command, check=True)
    os.makedirs(fuzz_dir, exist_ok=True)

    # 1-1) fuzzer takes a coverage and returns a coverage
    final_coverage_file: Filepath = Filepath(os.path.join(fuzz_dir, "final.coverage"))
    print(f"\n[DEBUG] === Reading fuzzer coverage from {final_coverage_file} ===")
    final_coverage = read_coverage(final_coverage_file)

    # 1-2) total coverage is updated with the new findings
    total_coverage = union_coverage(total_coverage, fuzzer_coverage)
    total_coverage_file: Filepath = Filepath(os.path.join(fuzz_dir, "total.coverage"))
    print(f"\n[DEBUG] === Writing merged coverage into {total_coverage_file} ===")
    write_coverage(total_coverage_file, total_coverage)

#
# End of the fuzzing loop
#

# TODO: From the final coverage, reduce likely hits
