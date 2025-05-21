import os
from coverage_utils import read_coverage, write_coverage

# Define your path pattern here
reduced_root = "fuzz/reduce0/"

# Read the original coverage
coverage = read_coverage("fuzz/fuzz0/final.coverage")

# Process: rewrite file paths
for origin, data in coverage.items():
    for pid, (status, filenames) in data.items():
        new_filenames = []
        for fname in filenames:
            basename = os.path.basename(fname)
            reduced_path = os.path.join(reduced_root, f"reduced_{pid}_{basename}")
            # Check if the reduced file exists
            if os.path.exists(reduced_path):
                new_filenames.append(reduced_path)
            else:
                new_filenames.append(fname)
        # Update filenames
        data[pid] = (status, new_filenames)

# Write the updated coverage to reduced.coverage
write_coverage("fuzz/reduce0/reduced.coverage", coverage)
