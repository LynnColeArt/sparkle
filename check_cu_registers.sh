#!/bin/bash
# Check CU enablement registers

echo "=== Checking AMD GPU CU Enablement Registers ==="
echo
echo "This will check if CUs are enabled for compute workloads"
echo "Running with sudo to access GPU registers..."
echo

# Check which card we're on
echo "1. GPU Detection:"
if [ -e /dev/dri/renderD129 ]; then
    echo "   Using renderD129 (likely integrated GPU)"
    CARD="card1"
elif [ -e /dev/dri/renderD128 ]; then
    echo "   Using renderD128"
    CARD="card0"
else
    echo "   No render device found!"
    exit 1
fi
echo

# Check GRBM_GFX_INDEX (broadcast mode)
echo "2. GRBM_GFX_INDEX (0x30800):"
echo "   Expected: 0xE0000000 (broadcast all)"
echo -n "   Actual:   "
sudo umr -r *.*.GRBM_GFX_INDEX 2>/dev/null | grep -o "0x[0-9a-fA-F]*" || echo "Failed to read"
echo

# Check COMPUTE_STATIC_THREAD_MGMT_SE0-3
echo "3. CU Enable Registers:"
echo "   Expected: 0xFFFFFFFF (all CUs enabled)"
for i in 0 1 2 3; do
    echo -n "   COMPUTE_STATIC_THREAD_MGMT_SE${i}: "
    sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE${i} 2>/dev/null | grep -o "0x[0-9a-fA-F]*" || echo "N/A"
done
echo

# Check if compute is even available
echo "4. Additional compute state:"
echo -n "   CP_MEC_ME1_PIPE0_ACTIVE: "
sudo umr -r *.*.CP_MEC_ME1_PIPE0_ACTIVE 2>/dev/null | grep -o "0x[0-9a-fA-F]*" || echo "N/A"
echo

# Try alternative register names for different GPU generations
echo "5. Alternative register checks (for different GPU gens):"
echo -n "   CP_COMPUTE_ENABLE: "
sudo umr -r *.*.CP_COMPUTE_ENABLE 2>/dev/null | grep -o "0x[0-9a-fA-F]*" || echo "N/A"
echo

echo "6. Checking shader enable state:"
echo -n "   SPI_SHADER_PGM_RSRC1_CS: "
sudo umr -r *.*.SPI_SHADER_PGM_RSRC1_CS 2>/dev/null | grep -o "0x[0-9a-fA-F]*" || echo "N/A"
echo

echo "=== Analysis ==="
echo "If GRBM_GFX_INDEX is not 0xE0000000, broadcast mode is not set."
echo "If COMPUTE_STATIC_THREAD_MGMT_SE* are 0x00000000, CUs are disabled."
echo "This would explain why compute waves aren't launching!"
echo
echo "To see all compute-related registers:"
echo "  sudo umr -r \"*.*.*COMPUTE*\" | grep -v \"0x00000000$\""