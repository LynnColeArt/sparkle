#!/bin/bash
# Debug script to check HQD and wave status with UMR (v2 - corrected syntax)

echo "=== Checking GPU info ==="
umr -i 1 -O verbose --print-asic

echo -e "\n=== Checking waves on Raphael iGPU ==="
umr -i 1 -O halt_waves,verbose --waves

echo -e "\n=== Checking compute rings ==="
# List all rings first
umr -i 1 --print-rings | grep -E "(compute|gfx)"

echo -e "\n=== Reading specific compute registers ==="
# Use the correct syntax: asicname.ipname.regname
# For Raphael (GFX10.3), we need to find the right prefixes
umr -i 1 -r *.CP_HQD_ACTIVE 2>/dev/null || echo "CP_HQD_ACTIVE not found"
umr -i 1 -r *.COMPUTE_PGM_LO 2>/dev/null || echo "COMPUTE_PGM_LO not found"
umr -i 1 -r *.COMPUTE_NUM_THREAD_X 2>/dev/null || echo "COMPUTE_NUM_THREAD_X not found"

echo -e "\n=== Checking shader controller registers ==="
# Try to find shader-related registers
umr -i 1 --scan-log | grep -i compute | head -20

echo -e "\n=== Checking if GFXOFF is disabled ==="
# GFXOFF can prevent wave execution
umr -i 1 -r *.RLC_CGCG_CGLS_CTRL 2>/dev/null || echo "RLC register not found"

echo -e "\n=== Listing available register blocks ==="
umr -i 1 --list-blocks | grep -E "(gc|cp|compute)" | head -20