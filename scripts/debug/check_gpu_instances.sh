#!/bin/bash
# Check which GPU instance is which

echo "=== System GPU info ==="
lspci | grep VGA

echo -e "\n=== DRM card info ==="
for card in /sys/class/drm/card*; do
    if [[ -f "$card/device/device" ]]; then
        device_id=$(cat "$card/device/device" 2>/dev/null)
        echo "$card: Device ID = $device_id"
    fi
done

echo -e "\n=== UMR instance list (requires root) ==="
echo "Run: sudo umr --list-instances"

echo -e "\n=== Quick test for each instance ==="
echo "Instance 0 (likely 7900 XT):"
echo "  sudo umr -i 0 --print-asic"
echo ""
echo "Instance 1 (likely Raphael iGPU):"
echo "  sudo umr -i 1 --print-asic"