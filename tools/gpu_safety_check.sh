#!/bin/bash
# GPU Safety Check Script

echo "🔍 Checking GPU safety..."

# Check if any GPU processes are stuck
if nvidia-smi &>/dev/null; then
    echo "NVIDIA GPU status:"
    nvidia-smi --query-gpu=utilization.gpu,utilization.memory,temperature.gpu --format=csv
fi

# Check AMD GPU
if [ -e /sys/class/drm/card1/device/gpu_busy_percent ]; then
    echo "AMD GPU utilization:"
    cat /sys/class/drm/card1/device/gpu_busy_percent 2>/dev/null || echo "N/A"
    echo "AMD GPU memory:"
    cat /sys/class/drm/card1/device/mem_info_vram_used 2>/dev/null || echo "N/A"
fi

# Check for zombie processes
zombies=$(ps aux | grep -E "defunct|<zombie>" | grep -v grep | wc -l)
if [ $zombies -gt 0 ]; then
    echo "⚠️  Found $zombies zombie processes"
fi

# Check system load
load=$(uptime | awk '{print $10}')
echo "System load average: $load"

echo "✅ Safety check complete"