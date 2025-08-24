#!/bin/bash
echo "=== Finding CP_SCRATCH registers ==="
echo "Searching for SCRATCH patterns:"
sudo umr -r *.SCRATCH* 2>/dev/null | head -20

echo ""
echo "=== Searching in gc registers ==="
sudo umr -r gc.*.SCRATCH* 2>/dev/null | head -10

echo ""
echo "=== Searching with different bases ==="
echo "mmCP_SCRATCH0:"
sudo umr -r *.mmCP_SCRATCH0 2>/dev/null

echo ""
echo "=== List all CP registers ==="
sudo umr -r *.CP_* 2>/dev/null | grep SCRATCH | head -10