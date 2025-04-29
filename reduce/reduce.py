import os
import subprocess
import shutil
from concurrent.futures import ThreadPoolExecutor, as_completed

# --- Configuration ---
INPUT_FILE = "reduce/cover-sl-filenames.output"  # Your input file path
WORK_DIR = "reductions"          # Temporary working directory
P4SPECTEC_DIR = "/Users/kunjeong/Docs/Development/Research/PLRG/p4cherry"
MAX_WORKERS = 4  # Number of parallel reductions

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
        reduced_path = os.path.join(WORK_DIR, f"r_{pid}_{base_name}")

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
        result = subprocess.run([
            "creduce", "--not-c", "--timeout", "10", interesting_test_subpath, copy_name
            ], cwd=WORK_DIR)

        if result.returncode == 0:
            # Rename the reduced file to indicate completion
            os.rename(temp_program_path, reduced_path)
            os.rename(temp_program_path + ".orig", temp_program_path)
            print(f"Reduced file has been renamed to {reduced_path}")
        else:
            print(f"creduce failed for {temp_program_path} with return code {result.returncode}")

        # Optionally remove the interestingness script after use
        # os.remove(interesting_test_path)

    except Exception as e:
        print(f"Failed to reduce pid={pid}, file={filename}: {e}")

# --- Read and process the input file ---
tasks = []
with open(INPUT_FILE, 'r') as f:
    lines = f.readlines()

for line in lines:
    parts = line.strip().split()
    if len(parts) < 2:
        continue

    pid, status = parts[0], parts[1]

    if status == "CM":
        filenames = parts[2:]  # Accepting 1, 2, or 3 filenames

        for filename in filenames:
            tasks.append((pid, filename))

# --- Run reductions in parallel ---
#with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
#    futures = [executor.submit(reduce_program, pid, filename) for pid, filename in tasks]
#    for future in as_completed(futures):
#        pass

# --- Run reductions sequentially on the main thread ---
for pid, filename in tasks:
    reduce_program(pid, filename)

print("All reductions complete.")
