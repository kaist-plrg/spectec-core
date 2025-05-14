from enum import Enum
from datetime import datetime
from typing import Dict, List, Tuple, TextIO, Union, NewType

#
# Coverage management
#

#
# Type aliases
#
PID = int
Origin = str  # function or relation that the PID belongs to
Filepath = NewType("Filepath", str)
Basename = NewType("Basename", str)
Directory = NewType("Directory", str)


class Status(Enum):
    HIT_LIKELY = 1
    HIT_UNLIKELY = 2
    CLOSE_MISS = 3
    COMPLETE_MISS = 4


CoverageEntry = Tuple[Status, List[Filepath]]
Coverage = Dict[Origin, Dict[PID, CoverageEntry]]
Reductions = Dict[PID, List[Filepath]]

def union(coverage_entry1: CoverageEntry, coverage_entry2: CoverageEntry) -> CoverageEntry:
    status1, files1 = coverage_entry1
    status2, files2 = coverage_entry2
    if status1 == Status.HIT_LIKELY and status2 == Status.HIT_LIKELY:
        return (Status.HIT_LIKELY, list(set(files1 + files2)))

    if status1 == Status.HIT_LIKELY and status2 == Status.HIT_UNLIKELY
        or status1 == Status.HIT_UNLIKELY and status2 == Status.HIT_LIKELY
        or status1 == Status.HIT_UNLIKELY and status2 == Status.HIT_UNLIKELY:
        return (Status.HIT_UNLIKELY, list(set(files1 + files2)))

    if (status1 == Status.HIT_LIKELY or status1 == Status.HIT_UNLIKELY)
        and (status2 == Status.CLOSE_MISS or status2 == Status.COMPLETE_MISS):
        return (status1, files1)

    if (status1 == Status.CLOSE_MISS or status1 == Status.COMPLETE_MISS)
        and (status2 == Status.HIT_LIKELY or status1 == Status.HIT_UNLIKELY):
        return (status2, files2)

    if status1 == Status.CLOSE_MISS and status2 == Status.CLOSE_MISS
        or status1 == Status.COMPLETE_MISS and status2 == Status.CLOSE_MISS
        or status1 == Status.CLOSE_MISS and status2 == Status.COMPLETE_MISS:
        return (Status.CLOSE_MISS, list(set(files1 + files2)))

    if status1 == Status.COMPLETE_MISS and status2 == Status.COMPLETE_MISS:
        return (Status.COMPLETE_MISS, [])


def read_coverage(coverage_file: Filepath) -> Coverage:
    coverage: Coverage = {}
    try:
        with open(coverage_file, "r", newline="") as file:
            for line in file:
                if line.startswith("#"):
                    continue

                data = line.strip().split(" ")
                pid: PID = int(data[0])
                status: Status = (
                    Status.HIT_LIKELY
                    if data[1] == "Hit_likely"
                    else (
                        Status.HIT_UNLIKELY
                        if data[1] == "Hit_unlikely"
                        else (
                            Status.CLOSE_MISS if len(data) > 3 else Status.COMPLETE_MISS
                        )
                    )
                )
                origin: Origin = data[2]
                filenames: List[Filepath] = (
                    [] if len(data) < 4 else [Filepath(file) for file in data[3:]]
                )

                if origin in coverage:
                    coverage_origin = coverage[origin]
                    coverage_origin[pid] = (status, filenames)
                else:
                    coverage[origin] = {pid: (status, filenames)}

            return coverage

    except FileNotFoundError:
        print(f"Error: File {coverage_file} not found.")
        return {}


def write_coverage(coverage_file: Filepath, coverage: Coverage) -> None:
    with open(coverage_file, "w") as file:
        for origin, data in coverage.items():
            for pid, (status, filenames) in data.items():
                status_str = (
                    "Hit_likely"
                    if status == Status.HIT_LIKELY
                    else (
                        "Hit_unlikely"
                        if status == Status.HIT_UNLIKELY
                        else "Miss" if status == Status.CLOSE_MISS else "Miss"
                    )
                )
                filenames_str = " ".join(filenames)
                file.write(f"{pid} {status_str} {origin} {filenames_str}\n")


def write_reductions(reductions_file: Filepath, reductions: Reductions) -> None:
    with open(reductions_file, "w") as file:
        for pid, files in reductions.items():
            file.write(f"{pid} {' '.join(files)}\n")


def read_reductions(reductions_file: Filepath) -> Reductions:
    reductions: Reductions = {}
    with open(reductions_file, "r") as file:
        for line in file:
            parts = line.strip().split(" ")
            if len(parts) < 2:
                continue
            pid: PID = int(parts[0])
            files: List[Filepath] = [Filepath(file) for file in parts[1:]]
            reductions[pid] = files
    return reductions


def log(msg: str, log_file: TextIO):
    now = datetime.now().strftime("[%H:%M:%S]")
    print(f"{now} {msg}", file=log_file)
    log_file.flush()
