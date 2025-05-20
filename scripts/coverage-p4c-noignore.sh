#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <p4c-pos> <p4c-neg> <p4c-all>"
    exit 1
fi

# Build the spec
make build-spec

# Inputs to the spec
DIR_INCLUDE=p4c/p4include
DIR_EXCLUDE=excludes
DIR_P4C_WELL=p4c/testdata/p4_16_samples
DIR_P4C_ILL=p4c/testdata/p4_16_errors

# Outputs
FILE_P4C_POS=$1
FILE_P4C_NEG=$2
FILE_P4C_ALL=$3

./p4spectec cover-sl spec/*.watsup -i $DIR_INCLUDE -e $DIR_EXCLUDE -d $DIR_P4C_WELL -cov $FILE_P4C_POS
./p4spectec cover-sl spec/*.watsup -i $DIR_INCLUDE -e $DIR_EXCLUDE -d $DIR_P4C_ILL -cov $FILE_P4C_NEG
./p4spectec cover-sl spec/*.watsup -i $DIR_INCLUDE -e $DIR_EXCLUDE -d $DIR_P4C_WELL -d $DIR_P4C_ILL -cov $FILE_P4C_ALL
