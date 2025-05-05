#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <gen dir> <fuel>"
    exit 1
fi

# Build the spec
make build-spec

# Inputs to the spec
DIR_INCLUDE=p4/testdata/arch
DIR_SEED=reductions
FILE_TARGET=reductions/reductions.target
FILE_IGNORE_REL=coverage/relation.ignore
FILE_IGNORE_FUNC=coverage/function.ignore

# Inputs
DIR_GEN=$1
FUEL=$2

./p4spectec testgen spec/*.watsup -i $DIR_INCLUDE -seed $DIR_SEED -target $FILE_TARGET -ignore $FILE_IGNORE_REL -ignore $FILE_IGNORE_FUNC -gen $DIR_GEN -silent -strict -fuel $FUEL
