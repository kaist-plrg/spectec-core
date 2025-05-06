import os
import subprocess
import shutil
import argparse
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from enum import Enum

#
# Parse command-line arguments
#

parser = argparse.ArgumentParser(description="Reduces all seed programs.")

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
parser.add_argument("--concurrent", action="store_true", help="Enable concurrency")
parser.add_argument(
    "--workers", type=int, default=1, help="Maximum number workers to use (default: 1)"
)
parser.add_argument(
    "--timeout",
    type=int,
    default=10,
    help="Timeout in seconds for creduce (default: 10 seconds)",
)

args = parser.parse_args()

#
# Configuration
#

P4SPECTEC_DIR = os.getenv("P4CHERRY_PATH")

# Configure the working directory
WORK_DIR = args.dir
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
CONCURRENT = args.concurrent
MAX_WORKERS = args.workers
if CONCURRENT:
    print(f"Running concurrently with {MAX_WORKERS} workers")
else:
    print("Running on main thread.")
TIMEOUT = args.timeout

#
# Coverage management
#

class Status(Enum):
    HIT_LIKELY = 1
    HIT_UNLIKELY = 2
    CLOSE_MISS = 3
    COMPLETE_MISS = 4

def read_coverage(coverage_file):
    try:
        with open(coverage_file, 'r', newline='') as file:
            lines = file.readlines()

            coverage = {}

            for line in lines:
                if line.startswith("#"):
                    continue

                data = line.strip().split(' ')
                pid = int(data[0])
                status = (
                    Status.HIT_LIKELY
                    if data[1] == "Hit_likely"
                    else Status.HIT_UNLIKELY
                    if data[1] == "Hit_unlikely"
                    else Status.CLOSE_MISS
                    if len(data) > 3
                    else Status.COMPLETE_MISS
                )
                origin = data[2]
                filenames = [] if len(data) < 4 else data[3:]

                if origin in coverage:
                    coverage_origin = coverage[origin]
                    coverage_origin[pid] = (status, filenames)
                else:
                    coverage[origin] = { pid : (status, filenames) }

            return coverage

    except FileNotFoundError:
        print(f"Error: File {file_path} not found.")
        return {}

def write_coverage(coverage_file, coverage):
    with open(coverage_file, 'w') as file:
        for origin, data in coverage.items():
            for pid, (status, filenames) in data.items():
                status_str = (
                    "Hit_likely"
                    if status == Status.HIT_LIKELY
                    else "Hit_unlikely"
                    if status == Status.HIT_UNLIKELY
                    else "Miss"
                    if status == Status.CLOSE_MISS
                    else "Miss"
                )
                filenames_str = " ".join(filenames)
                file.write(f"{pid} {status_str} {origin} {filenames_str}\n")

def write_target(target_file, target):
    with open(target_file, 'w') as file:
        for pid, filename in target.items():
            file.write(f"{pid} {filename}\n")

#
# Reduction task
#

def reduce_program(reduce_dir, pid, filename):
    try:
        # Setup the directory for reduction
        interesting_dir = os.path.join(reduce_dir, "interesting")
        os.makedirs(interesting_dir, exist_ok=True)

        # Create paths for the temporary program and interestingness test
        base_name = os.path.basename(filename)
        copy_name = f"o_{pid}_{base_name}"
        temp_program_path = os.path.join(reduce_dir, copy_name)
        interesting_test_path = os.path.join(interesting_dir, f"i_{pid}_{base_name}.sh")
        interesting_test_subpath = os.path.join("interesting", f"i_{pid}_{base_name}.sh")
        reduced_path = os.path.join(reduce_dir, f"__red_{pid}_{base_name}")

        # Preprocess the file instead of copying
        preprocess_command = [
                "cc", "-I", "p4/testdata/arch", "-undef", "-nostdinc", "-E", "-x", "c", filename
                ]
        with open(temp_program_path, 'w') as out_file:
            subprocess.run(preprocess_command, stdout=out_file, check=True)

        # Write the interestingness test script
        with open(interesting_test_path, 'w') as script_file:
            script_file.write(f"""#!/bin/bash
DIR=\"{P4SPECTEC_DIR}\"
$DIR/p4spectec interesting $DIR/spec/*.watsup -pid {pid} -p ./{copy_name}
                              """)

        # Make the script executable
        os.chmod(interesting_test_path, 0o755)

        # Run creduce
        print(f"Running creduce for pid={pid}, file={filename}")
        creduce_command = [
            "creduce", "--not-c", "--n", f"{args.cores}", "--timeout", "10", interesting_test_subpath, copy_name
            ] if args.cores else [
            "creduce", "--not-c", "--timeout", "10", interesting_test_subpath, copy_name
            ]
        
        start_time = time.time()
        try:
            result = subprocess.run(creduce_command, cwd=reduce_dir, timeout=args.timeout)  # timeout
            elapsed_time = time.time() - start_time

            if result.returncode == 0:
                os.rename(temp_program_path, reduced_path)
                os.rename(temp_program_path + ".orig", temp_program_path)
                print(f"Reduced file has been renamed to {reduced_path}")
                print(f"Elapsed time for pid={pid}, file={filename}: {elapsed_time:.2f} seconds")
                return reduced_path
            else:
                print(f"creduce failed for {temp_program_path} with return code {result.returncode}")
                print(f"Elapsed time for pid={pid}, file={filename}: {elapsed_time:.2f} seconds")
                return None 

        except subprocess.TimeoutExpired:
            elapsed_time = time.time() - start_time
            print(f"creduce timed out for {temp_program_path} after {elapsed_time:.2f} seconds. Saving current state as partially reduced file.")

            partial_reduced_path = os.path.join(reduce_dir, f"partial_{pid}_{base_name}")
            if os.path.exists(temp_program_path):
                os.rename(temp_program_path, partial_reduced_path)
                print(f"Partially reduced file saved as {partial_reduced_path}")
                return partial_reduced_path
            else:
                print(f"Warning: no partial file found to save for {temp_program_path}")
                return None

        return reduced_path

    except Exception as e:
        print(f"Failed to reduce pid={pid}, file={filename}: {e}")
        return None

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

    # Setup the directory for reduction
    name_reduce_campaign = f"reduce{loop_idx}"
    reduce_dir = os.path.join(WORK_DIR, name_reduce_campaign)
    os.makedirs(reduce_dir, exist_ok=True)

    # Reduce the smallest file for each close-missed phantom id
    target = {}
    for origin in coverage:
        for pid, (status, filenames) in coverage[origin].items():
            if status != Status.CLOSE_MISS:
                continue

            # Check if the filenames are valid
            filenames = [ filename for filename in filenames if os.path.isfile(filename) ]
            
            # Find the smallest unreduced file and reduce it
            filenames_unreduced = [ filename for filename in filenames if not filename.startswith("__red_") ]
            if not filenames_unreduced:
                print(f"No unreduced files found for pid={pid}, skipping reduction.")
                continue
            smallest_file = min(filenames_unreduced, key=os.path.getsize)
            reduced_smallest_file = reduce_program(reduce_dir, pid, smallest_file)
            if reduced_smallest_file is None:
                print(f"Failed to reduce {smallest_file}, skipping.")
                continue
            filenames.remove(smallest_file)
            filenames.append(reduced_smallest_file)

            # Update the coverage with the reduced file
            coverage[origin][pid] = (status, filenames)
            
            # Update the target
            target[pid] = reduced_smallest_file

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
        "1",
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
