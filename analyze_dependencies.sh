#!/bin/bash
# Analyze module dependencies to find circular references

echo "=== MODULE DEPENDENCY ANALYSIS ==="
echo ""
echo "Generating dependency map..."

# Create a detailed dependency report
for f in src/**/*.f90; do
  module_name=$(grep -E "^\s*module\s+" "$f" | head -1 | sed 's/^\s*module\s*//' | awk '{print $1}')
  if [ ! -z "$module_name" ]; then
    echo ""
    echo "Module: $module_name (from $f)"
    echo "Dependencies:"
    grep -E "^\s*use\s+" "$f" | sed 's/^\s*use\s*/  - /' | sed 's/,.*//'
  fi
done > dependency_map.txt

echo "Dependency map saved to dependency_map.txt"
echo ""

# Look for potential circular dependencies
echo "Checking for circular dependencies..."
echo ""

# Simple check: if module A uses B, does B use A?
for f in src/**/*.f90; do
  module_a=$(grep -E "^\s*module\s+" "$f" | head -1 | sed 's/^\s*module\s*//' | awk '{print $1}')
  if [ ! -z "$module_a" ]; then
    # Get all modules that A uses
    uses=$(grep -E "^\s*use\s+" "$f" | sed 's/^\s*use\s*//' | sed 's/,.*//' | grep -v iso_)
    
    for module_b in $uses; do
      # Find file containing module B
      b_file=$(grep -l "^\s*module\s\+$module_b\s*$" src/**/*.f90 2>/dev/null | head -1)
      
      if [ ! -z "$b_file" ]; then
        # Check if B uses A
        if grep -q "^\s*use\s\+$module_a" "$b_file"; then
          echo "🔄 CIRCULAR: $module_a ↔ $module_b"
          echo "   $module_a (in $f) uses $module_b"
          echo "   $module_b (in $b_file) uses $module_a"
          echo ""
        fi
      fi
    done
  fi
done

echo ""
echo "Analysis complete. Check dependency_map.txt for full details."