import sys

def read_coverage(file_path):
    try:
        with open(file_path, 'r', newline='') as file:
            lines = file.readlines()

            coverage = {}
            for line in lines:
                data = line.strip().split(' ')
                pid = int(data[0])
                status = data[1] == "Hit"
                coverage[pid] = status

            return coverage

    except FileNotFoundError:
        print(f"Error: File {file_path} not found.")
        return {}

if __name__ == "__main__":
    filename_a = sys.argv[1]
    coverage_a = read_coverage(filename_a)

    filename_b = sys.argv[2]
    coverage_b = read_coverage(filename_b)

    a_over_b = []
    b_over_a = []
    for pid in coverage_a:
        if pid not in coverage_b:
            print(f"PID {pid} is present in {filename_a} but not in {filename_b}.")
        else:
            status_a = coverage_a[pid]
            status_b = coverage_b[pid]
            if status_a and not status_b:
                a_over_b.append(pid)
            if not status_a and status_b:
                b_over_a.append(pid)
    
    print(f"--- Hit in {filename_a} but Miss in {filename_b}: {len(a_over_b)} PIDs ---")
    for i, pid in enumerate(a_over_b):
        if i % 10 == 0 and i != 0:
            print()
        print(pid, end=' ')

    print()

    print(f"--- Hit in {filename_b} but Miss in {filename_a}: {len(b_over_a)} PIDs ---")
    for i, pid in enumerate(b_over_a):
        if i % 10 == 0 and i != 0:
            print()
        print(pid, end=' ')
