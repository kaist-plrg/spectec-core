import os
import subprocess
import shutil
import argparse
import time
from coverage_utils import read_coverage, write_coverage, write_target, read_target, Status, log
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
    default=[ "coverage/relation.ignore", "coverage/function.ignore" ],
    help="List of ignore files for skipping phantom ids"
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
    help="Timeout in seconds for creduce (default: 40 seconds)",
)

args, unknown_args = parser.parse_known_args()

#
# Configuration
#
WORK_DIR = args.dir
P4SPECTEC_DIR = os.getenv('P4CHERRY_PATH')

# Configure working directory
if os.path.exists(WORK_DIR):
    print(f"Error: {WORK_DIR} already exists.")
    exit(1)
os.makedirs(WORK_DIR)
print(f"Working directory: {WORK_DIR}")
LOOPS = args.loops
if LOOPS < 1:
    print("Error: Loops must be greater than 0.")
    exit(1)
print(f"Loop count: {LOOPS}")

# Configure SpecTec
SPEC_DIR = args.spec
SPEC_FILES = [
    os.path.join(SPEC_DIR, file)
    for file in os.listdir(SPEC_DIR)
    if file.endswith(".watsup")
]
SPEC_FILES = sorted(SPEC_FILES)
print(f"Spec files: {SPEC_FILES}")
INCLUDE_DIR = args.include
print(f"Include directory: {INCLUDE_DIR}")
IGNORE_FILES = args.ignores
COVERAGE_FILE = args.coverage
if not os.path.isfile(COVERAGE_FILE):
    print(f"Error: Coverage file {COVERAGE_FILE} does not exist.")
    exit(1)

# Configuring C-Reduce
CORES = args.cores

#
# Main fuzzing loop
#

loop_idx = 0

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
    *[ cmd for file in IGNORE_FILES for cmd in ("-ignore", file) ],
    "-gen",
    WORK_DIR,
]

#
# Initial fuzzing iteration
#

name_fuzz_campaign = f"fuzz{loop_idx}"

spectec_init_command = spectec_command_template.copy() + [
    "-fuel",
    "1",
    "-warm",
    COVERAGE_FILE,
    "-name",
    name_fuzz_campaign
]

result = subprocess.run(spectec_init_command, check=True)

#
# Fuzzing loop: reduce and generate
#

while loop_idx < LOOPS:
    # Get and read the resultant coverage file
    name_fuzz_campaign = f"fuzz{loop_idx}"
    coverage_file = os.path.join(WORK_DIR, name_fuzz_campaign, "final.coverage")
    coverage = read_coverage(coverage_file)
    if loop_idx == 0:
        target = {}
    else:
        prev_target_file = os.path.join(WORK_DIR, f"reduce{loop_idx-1}", "reduced.target")
        target = read_target(prev_target_file)

    # Setup the directory for reduction
    name_reduce_campaign = f"reduce{loop_idx}"
    reduce_dir = os.path.join(WORK_DIR, name_reduce_campaign)
    os.makedirs(reduce_dir, exist_ok=True)

    global_log_path = os.path.join(reduce_dir, "reducer.log")
    with open(global_log_path, 'a') as global_log_file:
        # Reduce the smallest file for each close-missed phantom id
        for origin in coverage:
            for pid, (status, filenames) in coverage[origin].items():
                if status != Status.CLOSE_MISS:
                    continue
                if pid in target and target[pid].len() >= 3:
                    log(f"Skipping: Already reduced 3 files for pid={pid}", global_log_file)

                # Check if the filenames are valid
                filenames = [ filename for filename in filenames if os.path.isfile(filename) ]
                
                # Find the smallest unreduced file and reduce it
                filenames_unreduced = [ filename for filename in filenames if not os.path.basename(filename).startswith("reduced") ]
                if not filenames_unreduced:
                    log(f"Skipping: No unreduced files found for pid={pid}", global_log_file)
                    continue
                smallest_file = min(filenames_unreduced, key=os.path.getsize)
                reduced_file = reduce_program(reduce_dir, pid, smallest_file, P4SPECTEC_DIR, CORES, args.timeout, args.timeout_creduce )
                if reduced_file is None:
                    log(f"Failed to reduce {smallest_file} for pid={pid}", global_log_file)
                    continue
                filenames.remove(smallest_file)
                filenames.append(reduced_file)

                # Update the coverage with the reduced file
                coverage[origin][pid] = (status, filenames)
                
                # Update the target
                if pid in target :
                    target[pid] += reduced_file
                else:
                    target[pid] = [ reduced_file ]

        # Output the reduced files as a coverage file
        coverage_file = os.path.join(reduce_dir, "reduced.coverage")
        write_coverage(coverage_file, coverage)

        # Generate a target file
        target_file = os.path.join(reduce_dir, "reduced.target")
        write_target(target_file, target)

        # Fuzzing with the reduced files
        loop_idx += 1
        name_fuzz_campaign = f"fuzz{loop_idx}"

        spectec_fuzz_command = spectec_command_template.copy() + [
            "-fuel",
            "10",
            "-warm",
            coverage_file,
            "-target",
            target_file,
            "-name",
            name_fuzz_campaign,
        ]

        result = subprocess.run(spectec_fuzz_command, check=True)

#
# End of the fuzzing loop
#

# TODO: From the final coverage, reduce likely hits
