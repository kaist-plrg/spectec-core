import os
import argparse
from coverage_utils import read_coverage, Status, write_target


def find_unreduced(coverage_file, reduced_dir, unreduced_file, reduced_file):
    coverage = read_coverage(coverage_file)
    unreduced_targets = {}
    reduced_targets = {}

    for origin, data in coverage.items():
        for pid, (status, filenames) in data.items():
            if status == Status.CLOSE_MISS and filenames:
                existing_files = [fn for fn in filenames if os.path.isfile(fn)]
                if not existing_files:
                    print(f"Skipping pid={pid}: no valid file found")
                    continue
                smallest_file = min(existing_files, key=lambda fn: os.path.getsize(fn))

                basename = os.path.basename(smallest_file)
                reduced_path = os.path.join(reduced_dir, f"reduced_{pid}_{basename}")
                if os.path.exists(reduced_path):
                    reduced_targets[pid] = reduced_path
                else:
                    unreduced_targets[pid] = smallest_file

    print(f"Found {len(unreduced_targets)} unreduced CLOSE_MISS files.")
    print(f"Found {len(reduced_targets)} properly reduced CLOSE_MISS files.")

    write_target(unreduced_file, unreduced_targets)
    print(f"Saved unreduced targets to: {unreduced_file}")

    write_target(reduced_file, reduced_targets)
    print(f"Saved reduced targets to: {reduced_file}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Find unreduced CLOSE_MISS files and write reduction target file."
    )
    parser.add_argument("coverage_file", help="Path to the coverage file")
    parser.add_argument("reduced_dir", help="Directory containing reduced files")
    parser.add_argument(
        "--unreduced-file",
        default="unreduced.target",
        help="Path to save unreduced targets",
    )
    parser.add_argument(
        "--reduced-file", default="reduced.target", help="Path to save reduced targets"
    )
    args = parser.parse_args()
    find_unreduced(
        args.coverage_file, args.reduced_dir, args.unreduced_file, args.reduced_file
    )
