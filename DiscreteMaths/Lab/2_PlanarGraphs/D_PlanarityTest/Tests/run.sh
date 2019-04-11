#!/bin/bash
set -e

python3 test2.py > test
cat test | ./tested > .tested_out
diff -u .is_planar .tested_out
