#!/bin/bash
echo "=== Checking debugfs ring dumps ==="

echo "1. Finding DRI device:"
ls -la /sys/kernel/debug/dri/

echo -e "\n2. Checking for ring files:"
sudo ls /sys/kernel/debug/dri/*/amdgpu_ring_* 2>/dev/null | head -10

echo -e "\n3. GFX ring dump (card0):"
sudo cat /sys/kernel/debug/dri/0/amdgpu_ring_gfx_0 2>/dev/null | head -20

echo -e "\n4. GFX ring dump (card1):"  
sudo cat /sys/kernel/debug/dri/1/amdgpu_ring_gfx_0 2>/dev/null | head -20

echo -e "\n5. BO list info:"
sudo cat /sys/kernel/debug/dri/0/amdgpu_bo_list 2>/dev/null | head -10
sudo cat /sys/kernel/debug/dri/1/amdgpu_bo_list 2>/dev/null | head -10