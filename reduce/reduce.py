import os
import subprocess
import shutil
import argparse
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

parser = argparse.ArgumentParser(description="Reduces all seed programs.")
parser.add_argument('coverage', type=str, help='Path to coverage data')
parser.add_argument('dir', type=str, help='Path to working directory, where pre-processed p4 programs, interestingness tests and reduced copies are kept.')
parser.add_argument('--workers', type=int, default=1, help='Maximum number workers to use (default: 1)')
parser.add_argument('--cores', type=int, help='Number of cores for creduce (default: creduce finds an optimal setting)')
parser.add_argument('--concurrent', action='store_true', help='Enable concurrency')
parser.add_argument('--timeout', type=int, default=600, help='Timeout in seconds for creduce (default: 600 seconds)')
args = parser.parse_args()

if not os.path.isfile(args.coverage):
    print(f"Error: input file {args.coverage} does not exist.")
    exit(1)

# --- Configuration ---
COVERAGE = args.coverage
WORK_DIR = args.dir
P4SPECTEC_DIR = os.getenv('P4CHERRY_PATH')
MAX_WORKERS = args.workers  # Number of parallel reductions

# --- Print configuration ---
print(f"Coverage: {COVERAGE}")
print(f"Output directory: {WORK_DIR}")
if args.concurrent:
    print(f"Running concurrently with {MAX_WORKERS} workers")
else:
    print("Running on main thread.")

# --- Ensure working directory ---
os.makedirs(WORK_DIR, exist_ok=True)
interesting_dir = os.path.join(WORK_DIR, "interesting")
os.makedirs(interesting_dir, exist_ok=True)

# --- Define reduction task ---
def reduce_program(pid, filename):
    try:
        base_name = os.path.basename(filename)
        copy_name = f"o_{pid}_{base_name}"
        temp_program_path = os.path.join(WORK_DIR, copy_name)
        interesting_test_path = os.path.join(interesting_dir, f"i_{pid}_{base_name}.sh")
        interesting_test_subpath = os.path.join("interesting", f"i_{pid}_{base_name}.sh")
        reduced_path = os.path.join(WORK_DIR, f"r2_{pid}_{base_name}")

        # Skip if already reduced
        if os.path.exists(reduced_path):
            print(f"Skipping {reduced_path}, already reduced.")
            return
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
            result = subprocess.run(creduce_command, cwd=WORK_DIR, timeout=args.timeout)  # timeout
            elapsed_time = time.time() - start_time

            if result.returncode == 0:
                os.rename(temp_program_path, reduced_path)
                os.rename(temp_program_path + ".orig", temp_program_path)
                print(f"Reduced file has been renamed to {reduced_path}")
            else:
                print(f"creduce failed for {temp_program_path} with return code {result.returncode}")
            
            print(f"Elapsed time for pid={pid}, file={filename}: {elapsed_time:.2f} seconds")

        except subprocess.TimeoutExpired:
            elapsed_time = time.time() - start_time
            print(f"creduce timed out for {temp_program_path} after {elapsed_time:.2f} seconds. Saving current state as partially reduced file.")

            partial_reduced_path = os.path.join(WORK_DIR, f"partial_{pid}_{base_name}")
            if os.path.exists(temp_program_path):
                os.rename(temp_program_path, partial_reduced_path)
                print(f"Partially reduced file saved as {partial_reduced_path}")
            else:
                print(f"Warning: no partial file found to save for {temp_program_path}")

        # Optionally remove the interestingness script after use
        # os.remove(interesting_test_path)

    except Exception as e:
        print(f"Failed to reduce pid={pid}, file={filename}: {e}")

# --- Read and process the input file ---
tasks = []
with open(COVERAGE, 'r') as f:
    lines = f.readlines()

for line in lines:
    parts = line.strip().split()
    if len(parts) < 2:
        continue

    pid, status = parts[0], parts[1]

    if status == "Miss":
        filenames = parts[3:]  # Accepting 1, 2, or 3 filenames
        if filenames:
            existing_files = [fn for fn in filenames if os.path.isfile(fn)]
            not_reduced_files = [fn for fn in filenames if os.path.isfile(os.path.join(WORK_DIR, f"r2_{pid}_{os.path.basename(fn)}"))]
            if not existing_files:
                print(f"Skipping pid={pid}: no valid files found.")
                continue
            smallest_file = min(existing_files, key=lambda fn: os.path.getsize(fn))
            tasks.append((pid, smallest_file))

# --- Run reductions in parallel ---
if args.concurrent:
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = [executor.submit(reduce_program, pid, filename) for pid, filename in tasks]
        for future in as_completed(futures):
            pass

# --- Run reductions sequentially on the main thread ---
else:
    for pid, filename in tasks:
        reduce_program(pid, filename)

print("All reductions complete.")
