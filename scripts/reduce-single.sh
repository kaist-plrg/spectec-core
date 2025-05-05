#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <reduce_file> <reduce_pid> <reduce_dir>"
    exit 1
fi

export P4CHERRY_PATH=$(pwd)
echo "Setting P4CHERRY_PATH to $P4CHERRY_PATH"

# Build the spec
make build-spec

# File to reduce
FILE=$1
if [ ! -f "$FILE" ]; then
    echo "File $FILE does not exist."
    exit 1
fi

# Phantom ID to target
PID=$2

# Reduce directory
DIR_REDUCE=$3
if [ -d "$DIR_REDUCE" ]; then
    echo "Directory $DIR_REDUCE already exists."
    exit 1
fi
mkdir -p $DIR_REDUCE

python3 scripts/reduce.py --cores 8 --file $FILE --pid $PID $DIR_REDUCE
