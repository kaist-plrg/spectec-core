import re
from pathlib import Path
from typing import Dict, List, Set, Tuple

MutationStats = Dict[str, Tuple[int, Set[int]]]


def collect_mutation_hits(log_dir: Path) -> MutationStats:
    stats: MutationStats = {}
    mut_pattern = re.compile(r"hits\s+\{([^\}]+)\}.*\(COUNT\s+(\d+)\)\s+\(([^)]+)\)")

    for log_file in sorted(log_dir.glob("*.log")):
        with open(log_file) as f:
            for line in f:
                if "hits" not in line:
                    continue
                match = mut_pattern.search(line)
                if match:
                    pid_str, count_str, mutation = match.groups()
                    count = int(count_str)
                    pids = {int(p.strip()) for p in pid_str.split(", ")}

                    if mutation not in stats:
                        stats[mutation] = (0, set())

                    prev_count, prev_pids = stats[mutation]
                    stats[mutation] = (prev_count + count, prev_pids.union(pids))

    return stats


def load_logs(base_dir: Path) -> MutationStats:
    all_stats: MutationStats = {}
    i = 0

    while True:
        fuzz_dir = base_dir / f"fuzz{i}" / "log"
        phase_had_data = False

        # Fuzzer phase
        if fuzz_dir.exists():
            stats = collect_mutation_hits(fuzz_dir)
            for mutation, (count, pids) in stats.items():
                if mutation not in all_stats:
                    all_stats[mutation] = (0, set())
                prev_count, prev_pids = all_stats[mutation]
                all_stats[mutation] = (prev_count + count, prev_pids.union(pids))
            phase_had_data = True

        if not phase_had_data:
            break

        i += 1
    return all_stats


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

    parser = argparse.ArgumentParser(description="Summarize hits per mutation type.")
    parser.add_argument("dir", type=Path, help="Root directory for fuzzing campaign")
    args = parser.parse_args()

    base_dir = args.dir
    stats = load_logs(base_dir)

    for mutation, (count, pids) in stats.items():
        print(f"{mutation}, {count}")
        # print(f" > PIDs:\n  {', '.join(map(str, sorted(pids)))}")
