import sys

from enum import Enum
class Status(Enum):
    HIT_LIKELY = 1
    HIT_UNLIKELY = 2
    CLOSE_MISS = 3
    COMPLETE_MISS = 4

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
                status = Status.HIT_LIKELY if data[1] == "Hit_likely" else Status.HIT_UNLIKELY if data[1] == "Hit_unlikely" else Status.CLOSE_MISS if len(data) > 3 else Status.COMPLETE_MISS
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
    filename = sys.argv[1]
    coverage = read_coverage(filename)

    total_likely_hit = 0
    total_unlikely_hit = 0
    total_close_miss = 0
    total_complete_miss = 0

    for origin in coverage:
        print(f"=== {origin} ===")

        likely_hit = []
        unlikely_hit = []
        close_miss = []
        complete_miss = []

        for pid in coverage[origin]:
            status = coverage[origin][pid]
            if status == Status.HIT_LIKELY:
                likely_hit.append(pid)
            elif status == Status.HIT_UNLIKELY:
                unlikely_hit.append(pid)
            elif status == Status.CLOSE_MISS:
                close_miss.append(pid)
            elif status == Status.COMPLETE_MISS:
                complete_miss.append(pid)

        print(f"--- Likely Hit: {len(likely_hit)} ---")
        print_coverage(likely_hit)
        print(f"--- Unlikely Hit: {len(unlikely_hit)} ---")
        print_coverage(unlikely_hit)
        print(f"--- Close Miss: {len(close_miss)} ---")
        print_coverage(close_miss)
        print(f"--- Complete Miss: {len(complete_miss)} ---")
        print_coverage(complete_miss)

        total_likely_hit += len(likely_hit)
        total_unlikely_hit += len(unlikely_hit)
        total_close_miss += len(close_miss)
        total_complete_miss += len(complete_miss)

    print("=== Total ===")
    print(f"--- Likely Hit: {total_likely_hit} ---")
    print(f"--- Unlikely Hit: {total_unlikely_hit} ---")
    print(f"--- Close Miss: {total_close_miss} ---")
    print(f"--- Complete Miss: {total_complete_miss} ---")
