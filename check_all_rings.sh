#!/bin/bash
echo "=== Checking all GFX rings for our packet ==="
echo "Looking for: C0003704 (WRITE_DATA) and CAFEBABE"
echo

for ring in /sys/kernel/debug/dri/*/amdgpu_ring_gfx*; do
    if [ -f "$ring" ]; then
        echo "Checking $ring:"
        sudo od -Ax -tx4 -v "$ring" 2>/dev/null | grep -E "(c0003704|cafebabe|C0003704|CAFEBABE)" | head -5
        echo
    fi
done

echo "=== Checking card1 (dri/1) specifically ==="
echo "This should be the 7900 XT, not the iGPU:"
sudo od -Ax -tx4 -N 512 -v /sys/kernel/debug/dri/1/amdgpu_ring_gfx_0.0.0 2>/dev/null | head -20