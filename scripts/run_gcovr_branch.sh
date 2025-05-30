#!/bin/bash

gcovr -r . \
  --filter 'frontends/common/' \
  --filter 'frontends/p4/' \
  --filter 'midend/' \
  --txt-metric=branch \
  --gcov-ignore-errors=no_working_dir_found \
  --gcov-ignore-parse-errors=suspicious_hits.warn_once_per_file
