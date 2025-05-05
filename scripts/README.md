# Scripts for the Fuzzer

## Measuring the Coverage

`coverage-p4c.sh` measures the coverage of the p4c compiler test suite.
It measures three coverage results: positive test suite, negative test suite, and the two as a whole.
Note that we ignore some functions and relations listed in `coverage/*.ignore` files.

AT THE PROJECT ROOT, RUN:

```shell
$ scripts/coverage-p4c.sh <pos> <neg> <all>
```

## Comparing Two Coverage Files

`compare.py` takes two coverage files and compares them.

```shell
$ python3 scripts/compare.py <coverage_file_a> <coverage_file_b>
```

## Reducing Test Programs

`reduce.py` invokes `creduce` to reduce the test programs.
It runs in either batch mode, expecting a coverage file as input, or in single mode, expecting a single test program and a target phantom id.

AT THE PROJECT ROOT, RUN:

```shell
# Batch mode on p4c positive type checker tests
$ scripts/reduce-p4c-pos.sh <output_dir>

# Single mode on a test program and a target phantom id
$ scripts/reduce-single.sh <test_program> <target_phantom_id> <output_dir>
```
