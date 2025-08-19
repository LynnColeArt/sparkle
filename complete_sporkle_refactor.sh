#!/bin/bash
# Complete the Sparkle → Sporkle refactoring
# ==========================================
#
# Files have been renamed but internal references still need updating

set -e

echo "🚀 Completing Sparkle → Sporkle refactoring..."
echo ""

# Count changes for summary
changes=0

# Step 1: Update module names and uses
echo "📝 Step 1: Updating module names inside files..."

for file in $(find . -name "*.f90" -o -name "*.F90" | grep -v ".git/" | grep -v "reference/sparkle/" | sort); do
    # Skip if file doesn't exist
    [[ -f "$file" ]] || continue
    
    # Skip the kinds module itself
    if [[ "$file" == *"/kinds.f90" ]]; then
        continue
    fi
    
    # Count changes before modification
    before=$(grep -c "sparkle\|Sparkle\|SPARKLE\|iso_fortran_env" "$file" 2>/dev/null || echo 0)
    
    # Update module declarations and uses
    sed -i.bak \
        -e 's/module sparkle/module sporkle/g' \
        -e 's/end module sparkle/end module sporkle/g' \
        -e 's/use sparkle/use sporkle/g' \
        -e 's/Module Sparkle/Module Sporkle/g' \
        -e 's/End Module Sparkle/End Module Sporkle/g' \
        -e 's/Use Sparkle/Use Sporkle/g' \
        -e 's/MODULE SPARKLE/MODULE SPORKLE/g' \
        -e 's/END MODULE SPARKLE/END MODULE SPORKLE/g' \
        -e 's/USE SPARKLE/USE SPORKLE/g' \
        -e 's/sparkle_/sporkle_/g' \
        -e 's/Sparkle_/Sporkle_/g' \
        -e 's/SPARKLE_/SPORKLE_/g' \
        "$file"
    
    # Update iso_fortran_env to kinds (except in kinds.f90 itself)
    sed -i \
        -e 's/use, intrinsic :: iso_fortran_env, only:.*/use kinds/g' \
        -e 's/use iso_fortran_env, only:.*/use kinds/g' \
        -e 's/use iso_fortran_env/use kinds/g' \
        "$file"
    
    # Update type declarations
    sed -i \
        -e 's/real(real32)/real(sp)/g' \
        -e 's/real(real64)/real(dp)/g' \
        -e 's/integer(int32)/integer(i32)/g' \
        -e 's/integer(int64)/integer(i64)/g' \
        -e 's/real32/sp/g' \
        -e 's/real64/dp/g' \
        -e 's/int32/i32/g' \
        -e 's/int64/i64/g' \
        "$file"
    
    # Remove backup file
    rm -f "${file}.bak"
    
    # Count changes after modification
    after=$(grep -c "sparkle\|Sparkle\|SPARKLE" "$file" 2>/dev/null || echo 0)
    
    # Report if file was changed (check if we had any sparkle/iso_fortran_env before)
    if [[ $before -gt 0 ]]; then
        echo "  ✓ Updated: $file"
        ((changes++))
    fi
done

# Step 2: Update Makefiles
echo ""
echo "🔧 Step 2: Updating Makefiles..."

for file in $(find . -name "Makefile*" -type f | grep -v ".git/"); do
    [[ -f "$file" ]] || continue
    
    before=$(grep -c "sparkle\|Sparkle\|SPARKLE" "$file" 2>/dev/null || echo 0)
    
    sed -i.bak \
        -e 's/sparkle/sporkle/g' \
        -e 's/Sparkle/Sporkle/g' \
        -e 's/SPARKLE/SPORKLE/g' \
        "$file"
    
    rm -f "${file}.bak"
    
    after=$(grep -c "sparkle\|Sparkle\|SPARKLE" "$file" 2>/dev/null || echo 0)
    
    if [[ $before -ne $after ]]; then
        echo "  ✓ Updated: $file"
        ((changes++))
    fi
done

# Step 3: Update documentation
echo ""
echo "📚 Step 3: Updating documentation..."

for file in $(find . -name "*.md" -o -name "*.txt" | grep -v ".git/" | grep -v "node_modules"); do
    [[ -f "$file" ]] || continue
    
    before=$(grep -c "sparkle\|Sparkle" "$file" 2>/dev/null || echo 0)
    
    # More selective for docs to preserve context
    sed -i.bak \
        -e 's|github.com/LynnColeArt/Sparkle|github.com/LynnColeArt/Sporkle|g' \
        -e 's|/sparkle|/sporkle|g' \
        -e 's|sparkle_|sporkle_|g' \
        -e 's|Sparkle Framework|Sporkle Framework|g' \
        -e 's|# Sparkle|# Sporkle|g' \
        -e 's|^Sparkle |Sporkle |g' \
        "$file"
    
    rm -f "${file}.bak"
    
    after=$(grep -c "sparkle\|Sparkle" "$file" 2>/dev/null || echo 0)
    
    if [[ $before -ne $after ]]; then
        echo "  ✓ Updated: $file"
        ((changes++))
    fi
done

# Step 4: Summary
echo ""
echo "✅ Refactoring complete!"
echo ""
echo "Summary:"
echo "- Updated $changes files"
echo "- Module names: sparkle_* → sporkle_*"
echo "- Type system: iso_fortran_env → kinds module"
echo "- Type names: real64→dp, int32→i32, etc."
echo ""
echo "Next steps:"
echo "1. Review changes: git diff"
echo "2. Build project: make clean && make all"
echo "3. Run tests: make test"
echo "4. Commit: git add -A && git commit -m 'refactor: Complete Sparkle to Sporkle migration with kinds module'"