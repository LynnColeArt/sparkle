#!/bin/bash
echo "=== Debug HQD Access ==="
echo "1. Raw umr output for WPTR:"
sudo umr -r *.mmCP_HQD_PQ_WPTR_LO 2>&1

echo ""
echo "2. Check if gfx1036 is correct:"
sudo umr -r gfx1036.mmCP_HQD_PQ_WPTR_LO 2>&1

echo ""
echo "3. List available rings:"
sudo umr -lr 2>&1 | grep -E "(gfx|compute)" | head -10

echo ""
echo "4. Try mmreg access:"
sudo umr -r mmCP_HQD_PQ_WPTR_LO 2>&1

echo ""
echo "5. Check GPU state:"
sudo umr -O halt_waves -wa | head -20