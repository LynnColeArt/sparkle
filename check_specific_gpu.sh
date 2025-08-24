#!/bin/bash
# Check registers on specific GPU instance

echo "=== Checking specific GPU instance ==="

# First identify which GPU we're working with
echo "1. Available GPUs:"
sudo umr -l 2>/dev/null | grep -E "gfx|gpu" || echo "Failed to list GPUs"
echo

# Check card1 (renderD129 - Raphael iGPU)
echo "2. Checking card1 (Raphael iGPU):"
echo -n "   GRBM_GFX_INDEX:  "
sudo umr -i 1 -r gfx1036.GRBM_GFX_INDEX 2>/dev/null || sudo umr -i 1 -r *.GRBM_GFX_INDEX 2>/dev/null | head -1
echo -n "   THREAD_MGMT_SE0: "
sudo umr -i 1 -r gfx1036.COMPUTE_STATIC_THREAD_MGMT_SE0 2>/dev/null || sudo umr -i 1 -r *.COMPUTE_STATIC_THREAD_MGMT_SE0 2>/dev/null | head -1
echo -n "   THREAD_MGMT_SE1: "
sudo umr -i 1 -r gfx1036.COMPUTE_STATIC_THREAD_MGMT_SE1 2>/dev/null || sudo umr -i 1 -r *.COMPUTE_STATIC_THREAD_MGMT_SE1 2>/dev/null | head -1
echo

# Check if CUs are partially enabled
echo "3. Analyzing SE0 value 0x2049fecf:"
printf "   Binary: %032b\n" 0x2049fecf
echo "   This shows some CUs disabled on SE0!"
echo "   SE0 has only ~11 CUs enabled out of 32"
echo

echo "4. To fix: need GFX preamble to set THREAD_MGMT_SE0 = 0xffffffff"