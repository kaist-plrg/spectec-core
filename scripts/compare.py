import sys

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
                status = data[1] == "Hit"
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

if __name__ == "__main__":
    filename_a = sys.argv[1]
    coverage_a = read_coverage(filename_a)

    filename_b = sys.argv[2]
    coverage_b = read_coverage(filename_b)

    for origin in coverage_a:
        print(f"=== {origin} ===")

        a_hit_b_miss = []
        a_miss_b_hit = []
        both_miss = []

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

            if status_a and not status_b:
                a_hit_b_miss.append(pid)
            if not status_a and status_b:
                a_miss_b_hit.append(pid)
            if not status_a and not status_b:
                both_miss.append(pid)

        print(f"--- Hit in {filename_a} but Miss in {filename_b}: {len(a_hit_b_miss)} PIDs ---")
        i = 0
        for pid in a_hit_b_miss:
            if i % 10 == 0 and i != 0:
                print()
            print(pid, end=' ')
            i += 1
        if i % 10 != 0:
            print()

        print(f"--- Hit in {filename_b} but Miss in {filename_a}: {len(a_miss_b_hit)} PIDs ---")
        i = 0
        for pid in a_miss_b_hit:
            if i % 10 == 0 and i != 0:
                print()
            print(pid, end=' ')
            i += 1
        if i % 10 != 0:
            print()

        print(f"--- Miss in both: {len(both_miss)} PIDs ---")
        i = 0
        for pid in both_miss:
            if i % 10 == 0 and i != 0:
                print()
            print(pid, end=' ')
            i += 1
        if i % 10 != 0:
            print()
        print()
