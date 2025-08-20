#!/bin/bash
# Safe Sparkle → Sporkle refactoring
# ===================================

set -e

echo "🚀 Starting safe Sparkle → Sporkle refactoring..."
echo ""

# Initialize counters
total_files=0
updated_files=0

# Function to check if file needs updating
needs_update() {
    local file="$1"
    grep -q "sparkle\|Sparkle\|SPARKLE\|iso_fortran_env" "$file" 2>/dev/null
}

# Function to update a single file
update_file() {
    local file="$1"
    
    # Create backup
    cp "$file" "${file}.bak"
    
    # Apply all replacements
    sed -i \
        -e 's/module sparkle/module sporkle/g' \
        -e 's/end module sparkle/end module sporkle/g' \
        -e 's/use sparkle/use sporkle/g' \
        -e 's/sparkle_/sporkle_/g' \
        -e 's/Sparkle/Sporkle/g' \
        -e 's/SPARKLE/SPORKLE/g' \
        "$file"
    
    # Update iso_fortran_env to kinds
    if [[ ! "$file" == *"/kinds.f90" ]]; then
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
            "$file"
    fi
    
    # Check if file actually changed
    if diff -q "$file" "${file}.bak" >/dev/null 2>&1; then
        # No changes, restore original
        mv "${file}.bak" "$file"
        return 1
    else
        # Changes made, remove backup
        rm -f "${file}.bak"
        return 0
    fi
}

# Process Fortran files
echo "📝 Processing Fortran source files..."

while IFS= read -r file; do
    # Skip reference/sparkle directory
    if [[ "$file" == *"reference/sparkle/"* ]]; then
        continue
    fi
    
    ((total_files++))
    
    if needs_update "$file"; then
        if update_file "$file"; then
            echo "  ✓ Updated: $file"
            ((updated_files++))
        fi
    fi
done < <(find . -name "*.f90" -o -name "*.F90" | grep -v ".git/")

# Process Makefiles
echo ""
echo "🔧 Processing Makefiles..."

while IFS= read -r file; do
    ((total_files++))
    
    if grep -q "sparkle\|Sparkle\|SPARKLE" "$file" 2>/dev/null; then
        cp "$file" "${file}.bak"
        
        sed -i \
            -e 's/sparkle/sporkle/g' \
            -e 's/Sparkle/Sporkle/g' \
            -e 's/SPARKLE/SPORKLE/g' \
            "$file"
        
        if ! diff -q "$file" "${file}.bak" >/dev/null 2>&1; then
            echo "  ✓ Updated: $file"
            ((updated_files++))
            rm -f "${file}.bak"
        else
            mv "${file}.bak" "$file"
        fi
    fi
done < <(find . -name "Makefile*" -type f | grep -v ".git/")

# Process documentation
echo ""
echo "📚 Processing documentation..."

while IFS= read -r file; do
    ((total_files++))
    
    if grep -q "sparkle\|Sparkle" "$file" 2>/dev/null; then
        cp "$file" "${file}.bak"
        
        # Selective replacements for docs
        sed -i \
            -e 's|github.com/LynnColeArt/Sparkle|github.com/LynnColeArt/Sporkle|g' \
            -e 's|/sparkle|/sporkle|g' \
            -e 's|sparkle_|sporkle_|g' \
            -e 's|Sparkle Framework|Sporkle Framework|g' \
            -e 's|Project: Sparkle|Project: Sporkle|g' \
            -e 's|# Sparkle|# Sporkle|g' \
            "$file"
        
        if ! diff -q "$file" "${file}.bak" >/dev/null 2>&1; then
            echo "  ✓ Updated: $file"
            ((updated_files++))
            rm -f "${file}.bak"
        else
            mv "${file}.bak" "$file"
        fi
    fi
done < <(find . \( -name "*.md" -o -name "*.txt" \) | grep -v ".git/" | grep -v "node_modules")

# Summary
echo ""
echo "✅ Refactoring complete!"
echo ""
echo "Summary:"
echo "- Examined $total_files files"
echo "- Updated $updated_files files"
echo "- Module names: sparkle → sporkle"
echo "- Type system: iso_fortran_env → kinds"
echo ""
echo "Next steps:"
echo "1. Review changes: git diff"
echo "2. Build: make clean && make -f Makefile.smart"
echo "3. Run tests: make test"
echo "4. Commit: git add -A && git commit -m 'refactor: Migrate Sparkle to Sporkle with kinds module'"