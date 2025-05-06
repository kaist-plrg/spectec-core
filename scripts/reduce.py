import os
import subprocess
import time
import threading
import argparse
import shutil
from datetime import datetime


TARGET_SIZE = 70            # in bytes
RELAX_AFTER = 5             # seconds before we start relaxing
RELAX_FACTOR = 0.15         # how much we relax per second

def log(msg, log_file):
    now = datetime.now().strftime("[%H:%M:%S]")
    print(f"{now} {msg}", file=log_file)
    log_file.flush()

def monitor_file_size(process, file_path, orig_size, start_time, log_file):
    try:
        while process.poll() is None:
            if os.path.exists(file_path):
                new_size = os.path.getsize(file_path)
                rate = (orig_size - new_size) / orig_size
                elapsed = time.time() - start_time
                if rate > 0:
                    relax_amount = RELAX_FACTOR * max(0, (elapsed-RELAX_AFTER))
                    adjusted_target_size = TARGET_SIZE * (1 + relax_amount)
                    if (new_size <= adjusted_target_size):
                        log(f"Reached target: {new_size} bytes (target: {adjusted_target_size:.0f}), {rate:.2%} reduced", log_file)
                        process.terminate()
                        break
            time.sleep(1)  # check every 1 seconds
    except Exception as e:
        log(f"Monitor error: {e}", log_file)

# --- Define reduction task ---
def reduce_program(reduce_dir, pid, filename, p4spectec_dir, cores, timeout=10, timeout_creduce=40):
    interesting_dir = os.path.join(reduce_dir, "interesting")
    os.makedirs(interesting_dir, exist_ok=True)
    base_name = os.path.basename(filename)
    copy_name = f"o_{pid}_{base_name}"
    temp_path = os.path.join(reduce_dir, copy_name)
    orig_path = os.path.join(reduce_dir, f"{copy_name}.orig")
    interesting_test_path = os.path.join(interesting_dir, f"i_{pid}_{base_name}.sh")
    interesting_test_subpath = os.path.join("interesting", f"i_{pid}_{base_name}.sh")
    reduced_path = os.path.join(reduce_dir, f"reduced_{pid}_{base_name}")
    global_log_path = os.path.join(reduce_dir, "reducer.log")
    creduce_log_path = os.path.join(reduce_dir, f"creduce_{pid}_{base_name}.log")

    with open(global_log_path, 'a') as global_log_file:

        # Skip if already reduced
        if os.path.exists(reduced_path):
            log(f"Skipping {reduced_path}, file exists.", global_log_file)
            return

        preprocess_command = [
                "cc", "-I", "p4/testdata/arch", "-undef", "-nostdinc", "-E", "-x", "c", filename
                ]
        try:
            with open(temp_path, 'w') as out_file:
                subprocess.run(preprocess_command, stdout=out_file, check=True)
            orig_size = os.path.getsize(temp_path)

            # Write the interestingness test script
            with open(interesting_test_path, 'w') as script_file:
                script_file.write(f"""#!/bin/bash\nDIR=\"{p4spectec_dir}\"
    $DIR/p4spectec interesting $DIR/spec/*.watsup -pid {pid} -p ./{copy_name}""")
            # Make the script executable
            os.chmod(interesting_test_path, 0o755)

            # Run creduce
            log(f"Running creduce for pid={pid}, file={filename}", global_log_file)
            creduce_command = ["creduce", "--not-c"]
            if cores:
                creduce_command.extend(["--n", f"{cores}"])
            creduce_command.extend(["--timeout", f"{timeout}"])
            #creduce_command.extend(unknown_args)  # ← forward extra args
            creduce_command.extend([interesting_test_subpath, copy_name]) 
            
            start_time = time.time()
            
            with open(creduce_log_path, 'w') as creduce_log_file:
                proc = subprocess.Popen(creduce_command, cwd=reduce_dir, stdout=creduce_log_file, stderr=subprocess.STDOUT)
                monitor_thread = threading.Thread(
                    target=monitor_file_size,
                    args=(proc, temp_path, orig_size, start_time, global_log_file)
                )
                monitor_thread.start()
                try:
                    proc.wait(timeout=timeout_creduce)
                except subprocess.TimeoutExpired:
                    proc.terminate()
                    log(f"Hard timeout: {copy_name}", global_log_file)
                monitor_thread.join()

            if os.path.exists(temp_path):
                os.rename(temp_path, reduced_path)
                log(f"Reduced file saved as {reduced_path}", global_log_file)
            else:
                log(f"No reduced file produced for {base_name} against {pid}", global_log_file)
                reduced_path = None
            # Cleanup
            if os.path.exists(interesting_dir):
                shutil.rmtree(interesting_dir)
                log(f"Deleted {interesting_dir}", global_log_file)
            if os.path.exists(orig_path):
                os.remove(orig_path)
                log(f"Deleted original file {copy_name}.orig", global_log_file)

            return reduced_path
        except Exception as e:
            log(f"Failed to reduce {pid}, {filename}: {e}", global_log_file)
            return None

if __name__ == "__main__":
    P4SPECTEC_DIR = os.getenv('P4CHERRY_PATH')
    parser = argparse.ArgumentParser(description="Standalone reducer")
    group = parser.add_mutually_exclusive_group(required=True)
    parser.add_argument("dir", type=str, help="Working directory")
    group.add_argument("--coverage", type=str, help="Path to coverage file (batch mode)")
    group.add_argument("--file", type=str, help="Single file path (single mode)")
    parser.add_argument("--pid", type=str, help="Phantom ID (required if --file is used)")
    parser.add_argument("--cores", type=int, default=6, help="Number of creduce cores")
    parser.add_argument("--timeout-creduce", type=int, default=40, help="creduce timeout in seconds")
    args = parser.parse_args()

    if args.file:
        if not args.pid:
            parser.error("--pid is required when --file is used")
        reduce_program(
            reduce_dir=args.dir,
            pid=args.pid,
            filename=args.file,
            p4spectec_dir=P4SPECTEC_DIR,
            cores=args.cores,
            timeout=args.timeout_creduce
        )
    elif args.coverage:
        with open(args.coverage) as f:
            for line in f:
                if line.startswith("#"):
                    continue
                parts = line.strip().split()
                pid, status = parts[0], parts[1]
                if status != "Miss" or len(parts) <= 3:
                    continue
                filenames = parts[3:]
                for filename in filenames:
                    reduce_program(
                        reduce_dir=args.reduce_dir,
                        pid=pid,
                        filename=filename,
                        p4spectec_dir=args.p4spectec_dir,
                        cores=args.cores,
                        timeout_creduce=args.timeout_creduce
                    )
