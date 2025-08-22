#!/bin/bash
# Debug compute initialization issues

echo "=== Checking if we're actually submitting to compute ring ==="
echo "Our code submits to IP type: AMDGPU_HW_IP_COMPUTE (2)"
echo ""

echo "=== Running strace on our PM4 test to see ioctl calls ==="
strace -e ioctl ./test_pm4_mini_final 2>&1 | grep -E "(AMDGPU_CS|DRM)" | head -20

echo -e "\n=== Checking kernel logs for GPU errors ==="
dmesg | grep -E "(amdgpu|compute|gpu)" | tail -20

echo -e "\n=== Testing with libdrm's compute test for comparison ==="
if [ -f /usr/lib/x86_64-linux-gnu/libdrm/amdgpu_test ]; then
    echo "Running libdrm's amdgpu_test (basic compute test)..."
    /usr/lib/x86_64-linux-gnu/libdrm/amdgpu_test -s 1 -t 8 2>&1 | head -20
else
    echo "libdrm amdgpu_test not found at expected location"
fi