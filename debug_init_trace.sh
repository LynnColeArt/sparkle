#!/bin/bash
# Debug initialization trace script

echo "=== GPU Initialization Debug Trace ==="
echo

# 1. Check current GPU state registers
echo "1. Checking UCONFIG registers (need sudo):"
echo "   GRBM_GFX_INDEX:"
sudo umr -r *.*.GRBM_GFX_INDEX 2>/dev/null || echo "   (requires sudo)"
echo "   COMPUTE_STATIC_THREAD_MGMT_SE0:"
sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE0 2>/dev/null || echo "   (requires sudo)"
echo

# 2. Check kernel ring info
echo "2. Kernel ring info:"
if [ -f /sys/kernel/debug/dri/0/amdgpu_ring_info ]; then
    echo "Ring info from debugfs:"
    sudo cat /sys/kernel/debug/dri/0/amdgpu_ring_info 2>/dev/null | grep -A5 "comp" || echo "(requires debugfs)"
else
    echo "   (debugfs not mounted or no access)"
fi
echo

# 3. Check firmware loading
echo "3. Firmware status:"
dmesg | grep -i "amdgpu.*mec" | tail -5 || echo "   (no MEC firmware messages)"
echo

# 4. Check compute ring status via sysfs
echo "4. Compute ring status:"
for i in /sys/class/drm/card*/device/gpu_busy_percent; do
    if [ -f "$i" ]; then
        card=$(basename $(dirname $(dirname $i)))
        echo "   $card: $(cat $i 2>/dev/null || echo "N/A")"
    fi
done
echo

# 5. Check if any process is using compute
echo "5. Processes using GPU:"
if command -v nvidia-smi &> /dev/null; then
    nvidia-smi pmon -c 1 2>/dev/null | grep -v "nvidia-smi" || true
fi
if command -v radeontop &> /dev/null; then
    timeout 1 radeontop -d - -l 1 2>/dev/null | grep -E "gpu|compute" || echo "   (radeontop not available)"
fi
echo

# 6. Try to trace our test with strace
echo "6. System call trace of PM4 test (first 50 lines):"
echo "   Running: strace -e ioctl ./test_pm4_selftest 2>&1"
echo "   (This will show exact ioctl calls being made)"
echo

# 7. Check if compute is disabled in kernel params
echo "7. Kernel parameters:"
cat /proc/cmdline | grep -o "amdgpu[^ ]*" || echo "   (no amdgpu parameters)"
echo

echo "=== End of trace ==="
echo
echo "To get more detailed register dumps, run:"
echo "  sudo umr -O halt_waves -wa"
echo "  sudo umr -R gfx10*.*.COMPUTE*"
echo
echo "To monitor in real-time:"
echo "  watch -n 1 'sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE0'"