# Scripts for the Fuzzer

## Measuring the Coverage

`coverage-p4c.sh` measures the coverage of the p4c compiler test suite.
It measures three coverage results: positive test suite, negative test suite, and the two as a whole.
Note that we ignore some functions and relations listed in `coverage/*.ignore` files.
We also exclude P4 test programs listed in the `excludes` directory.

AT THE PROJECT ROOT, RUN:

```shell
$ scripts/coverage-p4c.sh <pos> <neg> <all>
```

## Comparing Two Coverage Files

`compare.py` takes two coverage files and compares them.

```shell
$ python3 scripts/compare.py <coverage_file_a> <coverage_file_b>
```
