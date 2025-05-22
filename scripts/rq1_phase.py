import re
from pathlib import Path
from typing import Dict, List, Set, Tuple

def count_reduced(base_dir: Path, phase: int) -> int:
    """counts the number of reductions for each phase"""
    log_file: Path = base_dir / f"reduce{phase}" / "reducer.log"
    with open(log_file, "r") as file:
        return sum(1 for line in file if "[DONE]" in line)


def count_fuzzer_generated(base_dir: Path, phase: int) -> int:
    """counts the number of generated illtyped programs for each phase"""
    program_dir: Path = base_dir / f"fuzz{phase}" / "illtyped"
    return len([f for f in program_dir.iterdir() if f.is_file() and f.suffix == ".p4"])


def print_per_phase(base_dir: Path) -> None:
    """Prints the number of reductions and generated illtyped programs for each phase"""
    i = 0
    while True:
        try:
            num_reduced = count_reduced(base_dir, i)
            num_generated = count_fuzzer_generated(base_dir, i)
            print(f"reduce{i}, {num_reduced}")
            print(f"fuzz{i}, {num_generated}")
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
