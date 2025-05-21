import argparse
from coverage_utils import read_target
from reduce import reduce_program


def run_reductions_from_target(
    target_file, reduce_dir, p4spectec_dir, cores=None, timeout_creduce=600
):
    """
    Reads a target file with (pid, file) pairs and runs reductions for each.
    """
    target = read_target(target_file)

    print(f"Found {len(target)} targets to reduce.")

    for pid, filename in target.items():
        print(f"Reducing pid={pid}, file={filename}")
        reduce_program(
            reduce_dir=reduce_dir,
            pid=pid,
            filename=filename,
            p4spectec_dir=p4spectec_dir,
            cores=cores,
            timeout=timeout_creduce,
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Batch reducer from a target file.")
    parser.add_argument(
        "target_file", help="Path to the target file (pid + filename per line)"
    )
    parser.add_argument("reduce_dir", help="Directory for reductions")
    parser.add_argument("--p4spectec-dir", required=True, help="Path to p4spectec")
    parser.add_argument(
        "--cores", type=int, default=None, help="Number of creduce cores"
    )
    parser.add_argument(
        "--timeout-creduce", type=int, default=600, help="creduce timeout in seconds"
    )
    args = parser.parse_args()

    run_reductions_from_target(
        target_file=args.target_file,
        reduce_dir=args.reduce_dir,
        p4spectec_dir=args.p4spectec_dir,
        cores=args.cores,
        timeout_creduce=args.timeout_creduce,
    )
