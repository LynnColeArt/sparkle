#!/bin/bash
# Test compute rings with UMR using correct ring names

echo "=== Testing wave monitoring on comp rings ==="
# comp_1.0.0 is the first compute ring on device 1
umr -i 1 -O halt_waves --waves comp_1.0.0 2>&1 | grep -v ERROR | head -10

echo -e "\n=== Checking if strace shows correct ioctl ==="
timeout 2 strace -e ioctl -f ./test_pm4_mini_final 2>&1 | grep -E "DRM_IOCTL_AMDGPU_CS|ip_type" || echo "No CS ioctl captured"

echo -e "\n=== Check dmesg for any GPU errors during test ==="
./test_pm4_mini_final
sleep 0.5
dmesg | tail -10 | grep -E "(amdgpu|gpu)" || echo "No recent GPU messages"

echo -e "\n=== Dump compute-related registers ==="
# Try to read compute shader program registers
umr -i 1 -r gfx1030.mmCOMPUTE_PGM_LO 2>&1 | grep -v "ERROR\|WARNING" || echo "Register not accessible"
umr -i 1 -r gfx1030.mmCOMPUTE_NUM_THREAD_X 2>&1 | grep -v "ERROR\|WARNING" || echo "Register not accessible"

echo -e "\n=== Check if MEC/pipe is enabled ==="
# MEC (Micro Engine Compute) status
umr -i 1 -r gfx1030.mmCP_MEC_ME1_PIPE0_INT_STATUS 2>&1 | grep -v "ERROR\|WARNING" || echo "MEC register not accessible"