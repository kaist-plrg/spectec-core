import re
from pathlib import Path
from typing import Dict, List, Set, Tuple
from datetime import datetime, timedelta


#
# Helper functions
#
def parse_hhmmss(s: str) -> datetime:
    return datetime.strptime(s, "%H:%M:%S")


def compute_duration(start: datetime, end: datetime) -> timedelta:
    """Computes duration, accounting for midnight wrap."""
    if end < start:
        end += timedelta(days=1)
    return end - start


#
# Counting generated/reduced programs
#
def count_reduced(base_dir: Path, phase: int) -> int:
    """counts the number of reductions for each phase"""
    log_file: Path = base_dir / f"reduce{phase}" / "reducer.log"
    with open(log_file, "r") as file:
        return sum(1 for line in file if "[DONE]" in line)


def count_fuzzer_generated(base_dir: Path, phase: int) -> int:
    """counts the number of generated illtyped programs for each phase"""
    program_dir: Path = base_dir / f"fuzz{phase}" / "illtyped"
    return len([f for f in program_dir.iterdir() if f.is_file() and f.suffix == ".p4"])


#
# Get duration of each phase
#
def get_fuzzer_duration(base_dir: Path, phase: int) -> timedelta:
    init_path = base_dir / f"fuzz{phase}" / "log" / "init.log"
    start_time = None
    with open(init_path, "r") as f:
        for line in f:
            if match := re.search(r"\[\[(\d{2}:\d{2}:\d{2})\]\]", line):
                start_time = parse_hhmmss(match.group(1))
    if start_time is None:
        raise ValueError(f"No timestamp found in {path}")

    fuel1_path = base_dir / f"fuzz{phase}" / "log" / "fuel1.log"
    end_time = None
    with open(fuel1_path, "r") as f:
        lines = f.readlines()
        for line in reversed(lines):
            if match := re.search(r"\[\[(\d{2}:\d{2}:\d{2})\]\]", line):
                end_time = parse_hhmmss(match.group(1))
    if end_time is None:
        raise ValueError(f"No timestamp found in {path}")

    return compute_duration(start_time, end_time)


def get_reducer_duration(base_dir: Path, phase: int) -> timedelta:
    path = base_dir / f"reduce{phase}" / "reducer.log"
    times = []
    with open(path, "r") as f:
        for line in f:
            if match := re.search(r"\[(\d{2}:\d{2}:\d{2})\]", line):
                times.append(parse_hhmmss(match.group(1)))
    if not times:
        raise ValueError(f"No timestamps found in {path}")
    return compute_duration(times[0], times[-1])


def print_per_phase(base_dir: Path) -> None:
    """Prints the number of reductions and generated illtyped programs for each phase"""
    i = 0
    while True:
        try:
            num_reduced = count_reduced(base_dir, i)
            num_generated = count_fuzzer_generated(base_dir, i)
            dur_reducer = get_reducer_duration(base_dir, i)
            dur_fuzzer = get_fuzzer_duration(base_dir, i)
            print(f"reduce{i}, {dur_reducer}, {num_reduced}")
            print(f"fuzz{i}, {dur_fuzzer}, {num_generated}")
            i += 1
        except FileNotFoundError:
            break


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Summarize generated/reduced programs per phase."
    )
    parser.add_argument("dir", type=Path, help="Root directory for fuzzing campaign")
    args = parser.parse_args()

    base_dir = args.dir

    print_per_phase(base_dir)
