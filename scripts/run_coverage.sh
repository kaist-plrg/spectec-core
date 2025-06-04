#!/bin/bash

# Function to print usage
print_usage() {
    echo "Usage: $0 [-o output_dir] directory1 [directory2 ...]"
    echo "Options:"
    echo "  -o output_dir    Specify output directory (default: coverage_output)"
    exit 1
}

# Set default output directory
OUTPUT_DIR="coverage_output"

# Parse command line options
while getopts "o:h" opt; do
    case $opt in
        o)
            OUTPUT_DIR="$OPTARG"
            ;;
        h)
            print_usage
            ;;
        \?)
            print_usage
            ;;
    esac
done

# Shift to remove processed options
shift $((OPTIND-1))

# Check if at least one directory is provided
if [ $# -eq 0 ]; then
    print_usage
fi

# Clean up existing .gcda files
echo "Initializing coverage..."
find ./p4c -name "*.gcda" -delete

# Create output directory for coverage data
mkdir -p "$OUTPUT_DIR"

# Function to run p4test on all .p4 files in a directory
run_p4tests() {
    local dir="$1"
    echo "Processing directory: $dir"
    
    # Find all .p4 files and run p4test
    find "$dir" -name "*.p4" -type f | while read -r p4_file; do
        echo "Running p4test on: $p4_file"
        p4test "$p4_file"
        if [ $? -ne 0 ]; then
            echo "Warning: p4test failed for $p4_file"
        fi
    done
}

# Process each directory provided as argument
for dir in "$@"; do
    if [ ! -d "$dir" ]; then
        echo "Warning: $dir is not a directory, skipping..."
        continue
    fi
    run_p4tests "$dir"
done

# Copy all .gcda files from /build to output directory preserving structure
echo "Copying coverage data files..."
gcda_count=$(find ./p4c -name "*.gcda" | wc -l)
echo "Found $gcda_count .gcda files to copy"
find ./p4c -name "*.gcda" -exec cp --parents {} "$OUTPUT_DIR" \;
echo "Copied $gcda_count files to $OUTPUT_DIR"

# Copy .gcno files
gcno_count=$(find ./p4c -name "*.gcno" | wc -l)
echo "Found $gcno_count .gcno files to copy"
find ./p4c -name "*.gcno" -exec cp --parents {} "$OUTPUT_DIR" \;
echo "Copied $gcno_count .gcno files to $OUTPUT_DIR"


# Generate coverage report
echo "Generating coverage report..."
gcovr --root "$OUTPUT_DIR" \
  --filter '.*frontends/common/' \
  --filter '.*frontends/p4/' \
  --filter '.*midend/' \
  --txt-metric=branch \
  --gcov-ignore-errors=no_working_dir_found \
  --gcov-ignore-parse-errors=suspicious_hits.warn_once_per_file \
  -o "$OUTPUT_DIR/coverage_report.gcovr"

echo "Coverage report generated at $OUTPUT_DIR/coverage_report.gcovr"
