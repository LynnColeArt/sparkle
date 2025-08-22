#!/bin/bash
# Script to help update tests to use unified submit API

echo "=== Analyzing test files for old submit API usage ==="
echo

# Find all tests using old submit functions
echo "Tests using sp_submit_ib:"
grep -l "sp_submit_ib(" tests/*.f90 2>/dev/null | grep -v "sp_submit_ib_with_bos" | grep -v "sp_submit_ib_ring"
echo

echo "Tests using sp_submit_ib_with_bo:"
grep -l "sp_submit_ib_with_bo(" tests/*.f90 2>/dev/null | grep -v "sp_submit_ib_with_bos"
echo

# Analyze patterns
echo "=== Common patterns found ==="
echo

echo "Pattern 1 - Simple sp_submit_ib (no data BOs):"
grep -h "sp_submit_ib(ctx" tests/*.f90 2>/dev/null | head -3
echo

echo "Pattern 2 - sp_submit_ib_with_bo (single data BO):"
grep -h "sp_submit_ib_with_bo(ctx" tests/*.f90 2>/dev/null | head -3
echo

echo "=== Suggested replacements ==="
echo "1. sp_submit_ib(ctx, ib_bo, size, fence)"
echo "   -> sp_submit_ib_with_bos(ctx, ib_bo, size, [c_ptr::], fence)"
echo
echo "2. sp_submit_ib_with_bo(ctx, ib_bo, size, data_bo, fence)"
echo "   -> sp_submit_ib_with_bos(ctx, ib_bo, size, [data_bo_ptr], fence)"
echo

# Count total files needing updates
total=$(grep -l "sp_submit_ib\|sp_submit_ib_with_bo" tests/*.f90 2>/dev/null | grep -v "sp_submit_ib_with_bos" | grep -v "sp_submit_ib_ring" | sort -u | wc -l)
echo "=== Total files needing updates: $total ==="