#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <reduce_dir>"
    exit 1
fi

export P4CHERRY_PATH=$(pwd)
echo "Setting P4CHERRY_PATH to $P4CHERRY_PATH"

# Build the spec
make build-spec

# Coverage file for the reduce targets
COVERAGE=coverage/p4c-pos.coverage
if [ ! -f "$COVERAGE" ]; then
    echo "Coverage file $COVERAGE does not exist."
    exit 1
fi

# Reduce directory
DIR_REDUCE=$1
if [ -d "$DIR_REDUCE" ]; then
    echo "Directory $DIR_REDUCE already exists."
    exit 1
fi
mkdir -p $DIR_REDUCE

python3 scripts/reduce.py --concurrent --coverage $COVERAGE $DIR_REDUCE
