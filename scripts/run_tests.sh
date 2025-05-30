#!/bin/bash

# Usage: ./run_p4test.sh /path/to/testdir

if [ $# -ne 1 ]; then
    echo "Usage: $0 /path/to/testdir"
    exit 1
fi

TEST_DIR="$1"
P4TEST_CMD="p4test"  # change to full path if needed

if [ ! -d "$TEST_DIR" ]; then
    echo "Error: $TEST_DIR is not a directory"
    exit 1
fi

PASS=0
FAIL=0

echo "Running p4test on .p4 files in $TEST_DIR"
echo "----------------------------------------"

for file in "$TEST_DIR"/*.p4; do
    if [ -f "$file" ]; then
        echo "Testing: $file"
        $P4TEST_CMD "$file"
        if [ $? -eq 0 ]; then
            echo "✅ Passed"
            ((PASS++))
        else
            echo "❌ Failed"
            ((FAIL++))
        fi
        echo
    fi
done

echo "----------------------------------------"
echo "Finshed testing."
echo "Passed: $PASS"
echo "Failed: $FAIL"

