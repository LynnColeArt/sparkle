#!/bin/bash
echo "=== Searching for INDIRECT_BUFFER pointing to our IB ==="
echo "Our IB VA: 0x800004000"
echo "Shifted for packet: (0x800004000 >> 8) = 0x8000040"
echo "Looking for low dword ~0x08000040 or 0x00800040"
echo

# Search for potential IB pointer patterns
echo "Searching for 08000040 pattern:"
grep -i "08000040\|00800040" /tmp/ring_dump.txt | head -10

echo -e "\nSearching for packets with opcode 0x3F (INDIRECT_BUFFER):"
grep "c0..3f00" /tmp/ring_dump.txt | head -10

echo -e "\nLooking at context around IT_INDIRECT_BUFFER (0x3F):"
grep -B2 -A5 "c0023f00" /tmp/ring_dump.txt | head -20

echo -e "\nChecking if there's a ring summary file:"
if [ -f /sys/kernel/debug/dri/1/amdgpu_ring_info ]; then
    echo "Found ring info file:"
    sudo grep -A10 "gfx" /sys/kernel/debug/dri/1/amdgpu_ring_info
else
    echo "No ring info file found"
fi