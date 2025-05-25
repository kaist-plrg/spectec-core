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

def hits(coverage):
    hits = set()

    for origin in coverage:
        for pid, status in coverage[origin].items():
            if status == Status.HIT_LIKELY or status == Status.HIT_UNLIKELY:
                hits.add(pid)

    return hits

def print_set(pids):
    i = 0
    for pid in pids:
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

    hits_a = hits(coverage_a)
    print(f"Number of hits in {filename_a}: {len(hits_a)}")
    
    hits_b = hits(coverage_b)
    print(f"Number of hits in {filename_b}: {len(hits_b)}")

    common_hits = hits_a.intersection(hits_b)
    print(f"Number of common hits: {len(common_hits)}")

    unique_hits_a = hits_a - common_hits
    print(f"Number of unique likely hits in {filename_a}: {len(unique_hits_a)}")

    unique_hits_b = hits_b - common_hits
    print(f"Number of unique likely hits in {filename_b}: {len(unique_hits_b)}")
