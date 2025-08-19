#!/bin/bash
# Refactor sparkle -> sporkle and iso_fortran_env -> kinds
# =========================================================
#
# This script performs two refactorings at once:
# 1. Renames sparkle to sporkle throughout the codebase
# 2. Migrates from iso_fortran_env to kinds module
#
# Usage: ./refactor_sparkle_to_sporkle.sh [--dry-run]

set -e

DRY_RUN=false
if [[ "$1" == "--dry-run" ]]; then
    DRY_RUN=true
    echo "🔍 DRY RUN MODE - No changes will be made"
fi

echo "🚀 Starting Sparkle → Sporkle refactoring..."
echo ""

# Function to perform sed replacement
do_sed() {
    local file="$1"
    local pattern="$2"
    local replacement="$3"
    
    if $DRY_RUN; then
        # Just show what would change
        if grep -q "$pattern" "$file" 2>/dev/null; then
            echo "  Would update: $file"
        fi
    else
        # Actually do the replacement
        sed -i.bak "$pattern" "$file" && rm -f "${file}.bak"
    fi
}

# Function to rename file
do_rename() {
    local old_name="$1"
    local new_name="$2"
    
    # Skip if source doesn't exist
    [[ -f "$old_name" ]] || return 0
    
    # Create parent directory if needed
    local new_dir=$(dirname "$new_name")
    [[ -d "$new_dir" ]] || mkdir -p "$new_dir"
    
    if $DRY_RUN; then
        echo "  Would rename: $old_name → $new_name"
    else
        git mv "$old_name" "$new_name" 2>/dev/null || mv "$old_name" "$new_name"
    fi
}

# Step 1: Update file contents
echo "📝 Step 1: Updating file contents..."

# Find all Fortran files
fortran_files=$(find . -name "*.f90" -o -name "*.F90" | grep -v ".git/" | sort)

for file in $fortran_files; do
    # Skip if file doesn't exist (might have been renamed)
    [[ -f "$file" ]] || continue
    
    # Skip the kinds module itself
    if [[ "$file" == *"/kinds.f90" ]]; then
        echo "  Skipping kinds.f90 (don't modify the kinds module itself)"
        continue
    fi
    
    # Replace sparkle with sporkle (case sensitive)
    do_sed "$file" "s/sparkle/sporkle/g"
    do_sed "$file" "s/Sparkle/Sporkle/g" 
    do_sed "$file" "s/SPARKLE/SPORKLE/g"
    
    # Replace iso_fortran_env imports with kinds
    do_sed "$file" "s/use, intrinsic :: iso_fortran_env, only:.*/use kinds/g"
    
    # Update type declarations
    do_sed "$file" "s/real(real32)/real(sp)/g"
    do_sed "$file" "s/real(real64)/real(dp)/g"
    do_sed "$file" "s/integer(int32)/integer(i32)/g"
    do_sed "$file" "s/integer(int64)/integer(i64)/g"
done

# Step 2: Rename files
echo ""
echo "📁 Step 2: Renaming files..."

# Find all files with sparkle in the name
sparkle_files=$(find . -name "*sparkle*" -type f | grep -v ".git/" | sort -r)

for file in $sparkle_files; do
    # Skip if already processed
    [[ -f "$file" ]] || continue
    
    # Generate new name
    new_name=$(echo "$file" | sed 's/sparkle/sporkle/g; s/Sparkle/Sporkle/g')
    
    # Only rename if names differ
    if [[ "$file" != "$new_name" ]]; then
        do_rename "$file" "$new_name"
    fi
done

# Step 3: Update Makefiles
echo ""
echo "🔧 Step 3: Updating Makefiles..."

makefiles=$(find . -name "Makefile*" -type f | grep -v ".git/")

for file in $makefiles; do
    do_sed "$file" "s/sparkle/sporkle/g"
    do_sed "$file" "s/Sparkle/Sporkle/g"
    do_sed "$file" "s/SPARKLE/SPORKLE/g"
done

# Step 4: Update documentation
echo ""
echo "📚 Step 4: Updating documentation..."

doc_files=$(find . -name "*.md" -o -name "*.txt" | grep -v ".git/" | grep -v "node_modules")

for file in $doc_files; do
    [[ -f "$file" ]] || continue
    
    # For docs, we might want to preserve some historical references
    # So we're more selective about what we replace
    do_sed "$file" "s|/sparkle|/sporkle|g"  # URLs and paths
    do_sed "$file" "s|sparkle_|sporkle_|g"  # Function/module names
    do_sed "$file" "s|Sparkle Framework|Sporkle Framework|g"
    do_sed "$file" "s|# Sparkle|# Sporkle|g"  # Headings
done

# Step 5: Summary
echo ""
echo "✅ Refactoring complete!"
echo ""

if $DRY_RUN; then
    echo "This was a dry run. To apply changes, run without --dry-run"
else
    echo "Summary of changes:"
    echo "- Updated module/function names: sparkle → sporkle"
    echo "- Migrated to kinds module for type safety"
    echo "- Renamed files to match new naming"
    echo ""
    echo "Next steps:"
    echo "1. Review changes: git diff"
    echo "2. Run tests: make test"
    echo "3. Commit: git add -A && git commit -m 'refactor: Rename Sparkle to Sporkle and migrate to kinds module'"
fi