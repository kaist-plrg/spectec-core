from enum import Enum
from datetime import datetime

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

def write_target(target_file, target_dict):
    with open(target_file, 'w') as f:
        for pid, files in target_dict.items():
            if isinstance(files, list):
                f.write(f"{pid} {' '.join(files)}\n")
            else:
                f.write(f"{pid} {files}\n")
        f.flush()

def read_target(target_file):
    pid_to_files = {}
    with open(target_file, 'r') as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) < 2:
                continue
            pid = parts[0]
            files = parts[1:]
            pid_to_files[pid] = files
    return pid_to_files

def log(msg, log_file):
    now = datetime.now().strftime("[%H:%M:%S]")
    print(f"{now} {msg}", file=log_file)
    log_file.flush()
