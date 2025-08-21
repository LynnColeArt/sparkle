#!/bin/bash
# Script to update files to use Mini's hardening modules
# This script shows what needs to be done - run with care!

echo "🔧 Updating codebase to use Mini's hardening modules..."
echo ""
echo "This script will:"
echo "1. Add 'use time_utils' to files using system_clock"
echo "2. Add 'use flopcount' to files with manual FLOP calculations"
echo "3. Fix GFLOPS literals (1.0e6 -> proper kind)"
echo ""
echo "Files that need updating:"
echo ""

# Files using system_clock without time_utils
echo "=== Files using system_clock ==="
grep -l "system_clock" src/*.f90 src/production/*.f90 examples/*.f90 2>/dev/null | \
  xargs grep -L "use time_utils" | head -10

echo ""
echo "=== Files with manual FLOP calculations ==="
grep -l "\* 2_int64" src/*.f90 src/production/*.f90 examples/*.f90 2>/dev/null | \
  xargs grep -L "use flopcount" | head -10

echo ""
echo "=== Files with unsafe GFLOPS literals ==="
grep -l "1\.0e6\|1e6" src/*.f90 src/production/*.f90 examples/*.f90 2>/dev/null | \
  grep -v "_real" | head -10

echo ""
echo "To update a file:"
echo "1. Add 'use time_utils, only: tic, toc_seconds' after other use statements"
echo "2. Replace 'call system_clock(start, rate)' with 'call tic(start)'"
echo "3. Replace timing calculations with 'elapsed = toc_seconds(start)'"
echo "4. Add 'use flopcount' and use conv2d_flops() for FLOP counting"
echo "5. Fix literals: 1.0e6 -> 1.0e6_real64 (or appropriate kind)"