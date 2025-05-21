import os
import subprocess
import time
import threading
import argparse
import shutil
from typedefs import *
from typing import Union, TextIO, Optional, NewType, List, Callable
from coverage_utils import log
from subprocess import Popen

TARGET_SIZE: int = 70  # in bytes
RELAX_AFTER: int = 3  # seconds before we start relaxing
RELAX_FACTOR: float = 0.15  # how much we relax per second


def set_pgid(pgid: int) -> Callable[[], None]:
    def preexec() -> None:
        os.setpgid(0, pgid)

    return preexec


def monitor_file_size(
    process: Popen,
    file_path: Filepath,
    orig_size: int,
    start_time: float,
    log_file: TextIO,
) -> None:
    try:
        while process.poll() is None:
            if os.path.exists(file_path):
                new_size = os.path.getsize(file_path)
                rate = (orig_size - new_size) / orig_size
                elapsed = time.time() - start_time
                if rate > 0:
                    relax_amount = RELAX_FACTOR * max(0, (elapsed - RELAX_AFTER))
                    adjusted_target_size = TARGET_SIZE * (1 + relax_amount)
                    if new_size <= adjusted_target_size:
                        log(
                            f"Reached target: {new_size} bytes (target: {adjusted_target_size:.0f}), {rate:.2%} reduced",
                            log_file,
                        )
                        process.terminate()
                        break
            time.sleep(1)
    except Exception as e:
        log(f"Monitor error: {e}", log_file)


def reduce_program(
    reduce_dir: Directory,
    reduced_files_dir: Directory,
    pid: Union[int, str],
    filename: Filepath,
    p4spectec_dir: Directory,
    cores: Optional[int],
    timeout: int = 10,
    timeout_creduce: int = 25,
    fuzz_end: bool = False,
    pgid: Optional[int] = None,
) -> Optional[Filepath]:

    interesting_dir: Directory = Directory(os.path.join(reduce_dir, "interesting"))
    creduce_log_dir: Directory = Directory(os.path.join(reduce_dir, "creduce"))
    os.makedirs(interesting_dir, exist_ok=True)
    os.makedirs(creduce_log_dir, exist_ok=True)
    os.makedirs(reduced_files_dir, exist_ok=True)

    base_name: Basename = Basename(os.path.basename(filename))
    copy_name: Basename = Basename(f"o_{pid}_{base_name}")
    temp_path: Filepath = Filepath(os.path.join(reduce_dir, copy_name))
    orig_path: Filepath = Filepath(os.path.join(reduce_dir, f"{copy_name}.orig"))
    interesting_test_path: Filepath = Filepath(
        os.path.join(interesting_dir, f"i_{pid}_{base_name}.sh")
    )
    reduced_path: Filepath = Filepath(
        os.path.join(reduced_files_dir, f"r_{pid}_{base_name}")
    )
    global_log_path: Filepath = Filepath(os.path.join(reduce_dir, "reducer.log"))
    creduce_log_path: Filepath = Filepath(
        os.path.join(creduce_log_dir, f"creduce_{pid}_{base_name}.log")
    )

    with open(global_log_path, "a") as global_log_file:

        if os.path.exists(reduced_path):
            log(f"Skipping {reduced_path}, file exists.", global_log_file)
            return None

        preprocess_command = [
            "cc",
            "-I",
            "p4/testdata/arch",
            "-undef",
            "-nostdinc",
            "-E",
            "-x",
            "c",
            filename,
        ]
        try:
            with open(temp_path, "w") as out_file:
                subprocess.run(preprocess_command, stdout=out_file, check=True)

            orig_size: int = os.path.getsize(temp_path)

            # final reduction reduces "ill-typed" "hits"
            # all other reductions reduce "well-typed" "close-misses"
            with open(interesting_test_path, "w") as script_file:
                script_file.write(
                    f"""#!/bin/bash\nDIR=\"{p4spectec_dir}\"\n$DIR/p4spectec interesting {"" if fuzz_end else "-well -close"} $DIR/spec/*.watsup -pid {pid} -p ./{copy_name}"""
                )
            os.chmod(interesting_test_path, 0o755)

            log(
                f"[START] Running creduce for pid={pid}, file={filename}",
                global_log_file,
            )

            creduce_command = ["creduce", "--not-c"]
            if cores:
                creduce_command.extend(["--n", str(cores)])
            creduce_command.extend(["--timeout", str(timeout)])
            interesting_test_subpath: Filepath = Filepath(
                os.path.join("interesting", f"i_{pid}_{base_name}.sh")
            )
            creduce_command.extend([interesting_test_subpath, copy_name])

            start_time = time.time()

            with open(creduce_log_path, "w") as creduce_log_file:
                proc: Popen = (
                    subprocess.Popen(
                        creduce_command,
                        preexec_fn=set_pgid(pgid),
                        cwd=reduce_dir,
                        stdout=creduce_log_file,
                        stderr=subprocess.STDOUT,
                    )
                    if pgid is not None
                    else subprocess.Popen(
                        creduce_command,
                        cwd=reduce_dir,
                        stdout=creduce_log_file,
                        stderr=subprocess.STDOUT,
                    )
                )
                monitor_thread = threading.Thread(
                    target=monitor_file_size,
                    args=(proc, temp_path, orig_size, start_time, global_log_file),
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
                log(f"[DONE] file saved as {reduced_path}", global_log_file)
            else:
                log(
                    f"No reduced file produced for {base_name} against {pid}",
                    global_log_file,
                )
                return None

            if os.path.exists(interesting_dir):
                shutil.rmtree(interesting_dir)
            if os.path.exists(orig_path):
                os.remove(orig_path)

            return reduced_path
        except Exception as e:
            log(f"Failed to reduce {pid}, {filename}: {e}", global_log_file)
            return None


#
# Takes a coverage file and reduces one close-missed program per pid,
# up to max_reductions_per_pid
def reduce_from_coverage(
    total_coverage: Coverage,
    fuzzer_coverage: Coverage,
    max_reductions_per_pid: int,
    reductions: Reductions,
    reduce_dir: Directory,
    reduced_files_dir: Directory,
    creduce_configs: CReduceConfigs,
    pgid: int,
) -> None:
    global_log_path: Filepath = Filepath(os.path.join(reduce_dir, "reducer.log"))
    with open(global_log_path, "a") as global_log_file:
        # Reduce the smallest file for each close-missed phantom id
        for origin in total_coverage:
            for pid, (status, filenames) in total_coverage[origin].items():
                if status != Status.CLOSE_MISS:
                    continue
                if (
                    max_reductions_per_pid > 0
                    and pid in reductions
                    and len(reductions[pid]) >= max_reductions_per_pid
                ):
                    log(
                        f"Skipping: Already reduced {max_reductions_per_pid} files for pid={pid}",
                        global_log_file,
                    )
                    continue

                # Check if filenames are valid
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

                # Select the smallest file and reduce it
                smallest_file: Filepath = min(filenames_unreduced, key=os.path.getsize)
                reducer_result: Optional[Filepath] = reduce_program(
                    reduce_dir,
                    reduced_files_dir,
                    pid,
                    smallest_file,
                    creduce_configs.p4spectec_dir,
                    creduce_configs.cores,
                    creduce_configs.timeout_interesting,
                    creduce_configs.timeout_creduce,
                    False,
                    pgid,
                )
                if reducer_result is None:
                    log(
                        f"Failed to reduce {smallest_file} for pid={pid}",
                        global_log_file,
                    )
                    continue
                else:
                    reduced_file: Filepath = Filepath(reducer_result)

                # update total coverage: substitute original with reduced file
                # !! MUTATION to total_coverage
                total_coverage[origin][pid][1].remove(smallest_file)
                total_coverage[origin][pid][1].append(reduced_file)

                fuzzer_coverage[origin][pid][1].append(reduced_file)

                # update reductions
                reductions.setdefault(pid, []).append(reduced_file)
    return None


def reduce_likely_hits(
    total_coverage: Coverage,
    reductions: Reductions,
    reduce_dir: Directory,
    reduced_files_dir: Directory,
    creduce_configs: CReduceConfigs,
) -> None:
    global_log_path: Filepath = Filepath(os.path.join(reduce_dir, "reducer.log"))
    with open(global_log_path, "a") as global_log_file:
        for origin in total_coverage:
            for pid, (status, filenames) in total_coverage[origin].items():
                if status != Status.HIT_LIKELY:
                    continue

                # Check if filenames are valid
                filenames_valid: List[Filepath] = [
                    Filepath(f) for f in filenames if os.path.isfile(f)
                ]

                # ALSO reduces already reduced files, as a clean up
                for filename in filenames_valid:
                    # reduce all unreduced files
                    reducer_result: Optional[Filepath] = reduce_program(
                        reduce_dir,
                        reduced_files_dir,
                        pid,
                        filename,
                        creduce_configs.p4spectec_dir,
                        creduce_configs.cores,
                        creduce_configs.timeout_interesting,
                        creduce_configs.timeout_creduce,
                        True,  # fuzz_end
                    )
                    if reducer_result is None:
                        log(
                            f"Failed to reduce {filename} for pid={pid}",
                            global_log_file,
                        )
                        continue
                    else:
                        reduced_file: Filepath = Filepath(reducer_result)
                    # update reductions
                    reductions.setdefault(pid, []).append(reduced_file)
        return None
