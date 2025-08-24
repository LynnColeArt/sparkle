#!/bin/bash
echo "=== Searching entire GFX ring for our packet ==="
echo "Looking for WRITE_DATA (0xc0003704) and CAFEBABE..."
echo

# Dump the entire ring and search
sudo od -Ax -tx4 -v /sys/kernel/debug/dri/1/amdgpu_ring_gfx_0.0.0 > /tmp/ring_dump.txt
echo "Ring dump size: $(wc -l /tmp/ring_dump.txt)"

echo -e "\nSearching for c0003704 (WRITE_DATA):"
grep -i "c0003704" /tmp/ring_dump.txt | head -5

echo -e "\nSearching for cafebabe:"
grep -i "cafebabe" /tmp/ring_dump.txt | head -5

echo -e "\nSearching for our control word (40010500):"
grep -i "40010500" /tmp/ring_dump.txt | head -5

# Also check if the ring wrapped around
echo -e "\nFirst 20 lines of ring:"
head -20 /tmp/ring_dump.txt

echo -e "\nLast 20 lines of ring:"
tail -20 /tmp/ring_dump.txt