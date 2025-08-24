#!/usr/bin/env bash
set -euo pipefail

echo "=== AMDGPU ring introspection ==="

# 1) Ensure debugfs is mounted
if ! mount | grep -q "/sys/kernel/debug "; then
  echo ">> Mounting debugfs..."
  sudo mount -t debugfs none /sys/kernel/debug
fi

# 2) List all DRM cards and map PCI -> dri index
echo
echo "== /dev/dri nodes =="
ls -l /dev/dri || true

echo
echo "== DRM cards and PCI addresses =="
for p in /sys/class/drm/card*/device; do
  card=$(basename "$(dirname "$p")")
  pci=$(basename "$(readlink -f "$p")")
  echo "$card  ->  $pci"
done

# 3) Probe each dri index for amdgpu ring files
echo
echo "== Searching ring files per dri index =="
for d in /sys/kernel/debug/dri/*; do
  [ -d "$d" ] || continue
  echo "-- $d --"
  sudo ls "$d"/amdgpu_ring_* 2>/dev/null || echo "  (no amdgpu_ring_* files here)"
done

# 4) Pick the likely card (first with ring files) and dump ring state
echo
target=""
for d in /sys/kernel/debug/dri/*; do
  comp=$(sudo ls "$d"/amdgpu_ring_* 2>/dev/null | head -n1 || true)
  if [ -n "$comp" ]; then target="$d"; break; fi
done

if [ -z "$target" ]; then
  echo "!! No ring files found in debugfs. Possible causes:"
  echo "   - wrong kernel/debugfs permissions (secure boot/lockdown)"
  echo "   - amdgpu not the driver for this device"
  echo "   - different dri index than you expect"
  exit 1
fi

echo "== Using $target =="
echo
echo "== Ring summaries =="
sudo awk 'FNR==1{print "\n### "FILENAME} {print}' "$target"/amdgpu_ring_* 2>/dev/null | sed 's/^/  /'

# 5) Optional: show GPU info to confirm ASIC (gfx, CU count)
echo
if [ -f "$target/amdgpu_gpu_info" ]; then
  echo "== GPU info =="
  sudo cat "$target/amdgpu_gpu_info" | sed 's/^/  /'
fi
