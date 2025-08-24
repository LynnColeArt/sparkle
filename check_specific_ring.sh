#!/bin/bash
echo "=== Checking specific GFX ring files ==="

echo "1. Card 0 (7900 XT) GFX ring:"
echo "File: /sys/kernel/debug/dri/0/amdgpu_ring_gfx_0.0.0"
sudo cat /sys/kernel/debug/dri/0/amdgpu_ring_gfx_0.0.0 2>/dev/null || echo "Not found"

echo -e "\n2. Card 1 (Raphael iGPU) GFX ring:"
echo "File: /sys/kernel/debug/dri/1/amdgpu_ring_gfx_0.0.0"  
sudo cat /sys/kernel/debug/dri/1/amdgpu_ring_gfx_0.0.0 2>/dev/null || echo "Not found"

echo -e "\n3. Alternative GFX ring names:"
sudo ls -la /sys/kernel/debug/dri/*/amdgpu_ring_gfx* 2>/dev/null

echo -e "\n4. Try direct paths:"
echo "Card 0 GFX 0.0.0:"
sudo cat /sys/kernel/debug/dri/0000:03:00.0/amdgpu_ring_gfx_0.0.0 2>/dev/null | head -20
echo -e "\nCard 1 GFX 0.0.0:"
sudo cat /sys/kernel/debug/dri/0000:13:00.0/amdgpu_ring_gfx_0.0.0 2>/dev/null | head -20