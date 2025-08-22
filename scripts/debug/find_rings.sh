#!/bin/bash
# Find available rings and test wave monitoring

echo "=== Finding available rings in debugfs ==="
for i in 0 1; do
    echo "Device $i (/sys/kernel/debug/dri/$i/):"
    ls /sys/kernel/debug/dri/$i/ | grep -E "(ring|compute|gfx)" | head -10
    echo ""
done

echo "=== Checking UMR for available commands ==="
umr --help 2>&1 | grep -E "(waves|ring|compute)" | head -10

echo -e "\n=== Testing specific ring syntax ==="
# Try to find the right syntax for compute rings
for ring in "gfx_0.0.0" "comp_1.0.0" "compute_1.0.0" "kiq_2.1.0"; do
    echo "Testing: umr -i 1 --ring-read $ring"
    umr -i 1 --ring-read $ring 2>&1 | head -3
done

echo -e "\n=== Checking if compute is enabled ==="
# Check if compute queues are actually enabled
cat /sys/class/drm/card1/device/enable 2>/dev/null || echo "enable file not found"

echo -e "\n=== Checking GPU power state ==="
cat /sys/class/drm/card1/device/power_dpm_force_performance_level 2>/dev/null || echo "power state not found"

echo -e "\n=== UMR GPU status ==="
umr -i 1 --gpu-scan | grep -E "(compute|COMPUTE|CU)" | head -10