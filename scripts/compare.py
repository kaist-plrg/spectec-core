import sys

from enum import Enum
class Status(Enum):
    HIT = 1
    CLOSE_MISS = 2
    COMPLETE_MISS = 3

def read_coverage(file_path):
    try:
        with open(file_path, 'r', newline='') as file:
            lines = file.readlines()

            coverage = {}

            for line in lines:
                if line.startswith("#"):
                    continue

                data = line.strip().split(' ')
                pid = int(data[0])
                status = Status.HIT if data[1] == "Hit" else Status.CLOSE_MISS if len(data) > 3 else Status.COMPLETE_MISS
                origin = data[2]

                if origin in coverage:
                    coverage_origin = coverage[origin]
                    coverage_origin[pid] = status
                else:
                    coverage[origin] = { pid : status }

            return coverage

    except FileNotFoundError:
        print(f"Error: File {file_path} not found.")
        return {}

def print_coverage(coverage):
    i = 0
    for pid in coverage:
        if i % 10 == 0 and i != 0:
            print()
        print(pid, end=' ')
        i += 1
    if i > 0 and i % 10 != 0:
        print()

if __name__ == "__main__":
    filename_a = sys.argv[1]
    coverage_a = read_coverage(filename_a)

    filename_b = sys.argv[2]
    coverage_b = read_coverage(filename_b)

    print(f"=== Comparing {filename_a} with {filename_b} ===")
    for origin in coverage_a:
        print(f"=== {origin} ===")

        a_hit_b_miss = []
        a_miss_b_hit = []
        a_close_miss_b_complete_miss = []
        a_complete_miss_b_close_miss = []
        both_close_miss = []
        both_complete_miss = []

        coverage_origin_a = coverage_a[origin]

        if origin not in coverage_b:
            print(f"Origin {origin} is present in {filename_a} but not in {filename_b}.")
            continue
        coverage_origin_b = coverage_b[origin]

        for pid in coverage_origin_a:
            if pid not in coverage_origin_b:
                print(f"PID {pid} is present in {filename_a} but not in {filename_b}.")
                continue
            status_a = coverage_origin_a[pid]
            status_b = coverage_origin_b[pid]

            if status_a == Status.HIT and status_b == Status.HIT:
                continue
            if status_a == Status.HIT and (status_b == Status.CLOSE_MISS or status_b == Status.COMPLETE_MISS):
                a_hit_b_miss.append(pid)
            if (status_a == Status.CLOSE_MISS or status_a == Status.COMPLETE_MISS) and status_b == Status.HIT:
                a_miss_b_hit.append(pid)
            if status_a == Status.CLOSE_MISS and status_b == Status.COMPLETE_MISS:
                a_close_miss_b_complete_miss.append(pid)
            if status_a == Status.COMPLETE_MISS and status_b == Status.CLOSE_MISS:
                a_complete_miss_b_close_miss.append(pid)
            if status_a == Status.CLOSE_MISS and status_b == Status.CLOSE_MISS:
                both_close_miss.append(pid)
            if status_a == Status.COMPLETE_MISS and status_b == Status.COMPLETE_MISS:
                both_complete_miss.append(pid)

        print(f"--- Hit / Miss : {len(a_hit_b_miss)} PIDs ---")
        print_coverage(a_hit_b_miss)

        print(f"--- Miss / Hit : {len(a_miss_b_hit)} PIDs ---")
        print_coverage(a_miss_b_hit)

        print(f"--- Close Miss / Complete Miss : {len(a_close_miss_b_complete_miss)} PIDs ---")
        print_coverage(a_close_miss_b_complete_miss)

        print(f"--- Complete Miss / Close Miss : {len(a_complete_miss_b_close_miss)} PIDs ---")
        print_coverage(a_complete_miss_b_close_miss)

        print(f"--- Close Miss / Close Miss : {len(both_close_miss)} PIDs ---")
        print_coverage(both_close_miss)

        print(f"--- Complete Miss / Complete Miss : {len(both_complete_miss)} PIDs ---")
        print_coverage(both_complete_miss)
