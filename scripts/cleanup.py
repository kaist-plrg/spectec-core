import argparse
import os
import subprocess
from typedefs import *
from typing import List, Set
from pathlib import Path
from reduce import reduce_likely_hits
from coverage_utils import write_reductions, read_coverage


def find_coverage_dirs(base_dir: Directory, seed_dir: Directory) -> List[Directory]:
    """Collect all directories with .p4 files for coverage analysis."""
    base_path: Path = Path(base_dir)
    coverage_dirs: Set[Directory] = set()

    # 1. All fuzz*/illtyped dirs
    for subdir in base_path.glob("fuzz*/illtyped"):
        if subdir.is_dir():
            coverage_dirs.add(Directory(str(subdir.resolve())))

    # 2. All fuzz*/welltyped dirs
    for subdir in base_path.glob("fuzz*/welltyped"):
        if subdir.is_dir():
            coverage_dirs.add(Directory(str(subdir.resolve())))

    # 3. reduced/ directory
    reduced_dir = base_path / "reduced"
    if reduced_dir.is_dir():
        coverage_dirs.add(Directory(str(reduced_dir.resolve())))

    coverage_dirs.add(seed_dir)

    return sorted(coverage_dirs)


def collect_p4_files(dirs: List[Directory]) -> List[Filepath]:
    """Collect all .p4 files from the given directories."""
    p4_files: List[Filepath] = []
    for directory in dirs:
        for file in Path(directory).glob("*.p4"):
            if file.is_file():
                p4_files.append(Filepath(str(file.resolve())))
    return sorted(p4_files)


def run_coverage(
    work_dir: Directory,
    spec_dir: Directory,
    include: Directory,
    exclude: Directory,
    ignore_files: List[Filepath],
    testdata: Directory,
    output_path: Filepath,
) -> None:

    SPEC_FILES: List[Filepath] = [
        Filepath(os.path.join(spec_dir, file))
        for file in os.listdir(spec_dir)
        if file.endswith(".watsup")
    ]
    SPEC_FILES = sorted(SPEC_FILES)
    print(f"[CONFIG] Spec files: {SPEC_FILES}")

    coverage_dirs = find_coverage_dirs(work_dir, testdata)
    print(f"\n[INFO] Collecting from directories: {coverage_dirs}")

    p4_files = collect_p4_files(coverage_dirs)
    print(f"\n[INFO] Collected {len(p4_files)} .p4 files for coverage.")

    coverage_command = [
        "./p4spectec",
        "cover-sl",
        *SPEC_FILES,
        "-i",
        include,
        "-e",
        exclude,
        "-d",
        testdata,
        *[cmd for file in coverage_dirs for cmd in ["-d", file]],
        *[cmd for file in ignore_files for cmd in ["-ignore", file]],
        "-cov",
        output_path,
    ]
    print(f"\n[INFO] Running coverage command with {len(p4_files)} files...")
    subprocess.run(coverage_command, check=True)


def reduce_final(
    work_dir: Directory, total_coverage: Coverage, creduce_configs: CReduceConfigs
) -> None:
    print(f"\n[INFO] === Starting reduce_final ===")
    reduce_dir = Directory(os.path.join(work_dir, "reduce_final"))
    os.makedirs(reduce_dir, exist_ok=True)

    reduced_likely_hits_dir: Directory = Directory(
        os.path.join(work_dir, "likely_hits")
    )
    os.makedirs(reduced_likely_hits_dir, exist_ok=True)

    reductions: Reductions = {}
    reduce_likely_hits(
        total_coverage,
        reductions,
        reduce_dir,
        reduced_likely_hits_dir,
        creduce_configs,
    )

    print(f"\n[INFO] === Finished reduce_final with {len(reductions)} reductions ===")
    # Log reductions
    reductions_file = Filepath(os.path.join(reduce_dir, "reduced.reductions"))
    write_reductions(reductions_file, reductions)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Measure coverage from generated fuzz directories."
    )
    parser.add_argument("dir", type=str, help="Path to the working directory")
    parser.add_argument(
        "--spec", type=Directory, default="spec", help="Spec directory for SpecTec"
    )
    parser.add_argument(
        "--include",
        type=Directory,
        default="p4c/p4include",
        help="Include directory for SpecTec",
    )
    parser.add_argument(
        "--exclude",
        type=Directory,
        default="excludes",
        help="Exclude directory for P4 tests",
    )
    parser.add_argument(
        "--ignores",
        nargs="*",
        type=Directory,
        default=["coverage/relation.ignore", "coverage/function.ignore"],
        help="List of ignore files for skipping phantom ids",
    )
    parser.add_argument(
        "--testdata",
        type=Directory,
        default="p4c/testdata/p4_16_samples",
        help="base testdata for fuzzing to include in coverage",
    )
    args = parser.parse_args()

    SPEC_DIR: Directory = Directory(args.spec)
    if not os.path.isdir(SPEC_DIR):
        print(f"Error: Spec directory {SPEC_DIR} does not exist.")
        exit(1)

    INCLUDE_DIR: Directory = Directory(args.include)
    if not os.path.isdir(INCLUDE_DIR):
        print(f"Error: Include directory {INCLUDE_DIR} does not exist.")
        exit(1)
    print(f"[CONFIG] Include directory: {INCLUDE_DIR}")

    EXCLUDE_DIR: Directory = Directory(args.exclude)
    if not os.path.isdir(EXCLUDE_DIR):
        print(f"Error: Exclude directory {EXCLUDE_DIR} does not exist.")
        exit(1)
    print(f"[CONFIG] Exclude directory: {EXCLUDE_DIR}")

    TESTDATA_DIR: Directory = Directory(args.testdata)
    if not os.path.isdir(TESTDATA_DIR):
        print(f"Error: Testdata directory {TESTDATA_DIR} does not exist.")
        exit(1)
    print(f"[CONFIG] Testdata directory: {TESTDATA_DIR}")

    IGNORE_FILES: List[Filepath] = args.ignores
    WORK_DIR: Directory = Directory(args.dir)

    # P4CHERRY_PATH must be set
    if os.getenv("P4CHERRY_PATH") is None:
        print("Error: P4CHERRY_PATH environment variable is not set.")
        exit(1)
    P4SPECTEC_DIR: Directory = Directory(str(os.getenv("P4CHERRY_PATH")))
    print(f"[CONFIG] P4CHERRY_PATH: {P4SPECTEC_DIR}")
    print(f"\n[INFO] === Starting cleanup ===")
    OUTPUT_PATH: Filepath = Filepath(os.path.join(WORK_DIR, "total.coverage"))

    # Default configurations for CReduce
    C_REDUCE_CONFIGS = CReduceConfigs(
        p4spectec_dir=P4SPECTEC_DIR,
        cores=None,
        timeout_interesting=10,
        timeout_creduce=25,
    )
    run_coverage(
        WORK_DIR,
        SPEC_DIR,
        INCLUDE_DIR,
        EXCLUDE_DIR,
        IGNORE_FILES,
        TESTDATA_DIR,
        OUTPUT_PATH,
    )
    total_coverage: Coverage = read_coverage(OUTPUT_PATH)
    reduce_final(WORK_DIR, total_coverage, C_REDUCE_CONFIGS)
