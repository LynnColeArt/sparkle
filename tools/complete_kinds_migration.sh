#!/bin/bash
# Complete the kinds module migration for remaining files

echo "🔄 Completing kinds module migration..."
echo "====================================="
echo ""

# Count files before
count_before=$(grep -r "iso_fortran_env" . --include="*.f90" | grep -v "kinds.f90" | wc -l)
echo "Files using iso_fortran_env: $count_before"
echo ""

# Function to migrate a file
migrate_file() {
    local file=$1
    echo "  Migrating: $file"
    
    # Create backup
    cp "$file" "$file.bak"
    
    # Add kinds module if not present
    if ! grep -q "use kinds" "$file"; then
        # Add after program/module statement
        sed -i '/^\s*\(program\|module\|subroutine\|function\)/,/^\s*implicit none/{
            /implicit none/i\  use kinds
        }' "$file"
    fi
    
    # Replace iso_fortran_env imports
    sed -i 's/use, intrinsic :: iso_fortran_env, only: error_unit/use kinds\n  use, intrinsic :: iso_fortran_env, only: error_unit/' "$file"
    sed -i 's/use iso_fortran_env, only: int64/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int32/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: real64/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: real32/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int8/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int32, int64/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int64, real64/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: real64, real32/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int32, int64, real32/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int32, int64, real32, real64/use kinds/' "$file"
    sed -i 's/use iso_fortran_env, only: int32, int64, real32, int8/use kinds/' "$file"
    
    # Replace type declarations
    sed -i 's/integer(int64)/integer(i64)/g' "$file"
    sed -i 's/integer(int32)/integer(i32)/g' "$file"
    sed -i 's/integer(int8)/integer(i8)/g' "$file"
    sed -i 's/real(real64)/real(dp)/g' "$file"
    sed -i 's/real(real32)/real(sp)/g' "$file"
    
    # Replace literals
    sed -i 's/_int64/_i64/g' "$file"
    sed -i 's/_int32/_i32/g' "$file"
    sed -i 's/_int8/_i8/g' "$file"
    sed -i 's/_real64/_dp/g' "$file"
    sed -i 's/_real32/_sp/g' "$file"
    
    # Clean up any duplicate 'use kinds' statements
    awk '!seen[$0]++ || !/use kinds/' "$file" > "$file.tmp" && mv "$file.tmp" "$file"
    
    # Remove backup if successful
    if [ $? -eq 0 ]; then
        rm "$file.bak"
    else
        echo "    ⚠️  Migration failed, backup kept at $file.bak"
    fi
}

# Find and migrate all files
echo "Migrating files..."
for file in $(grep -r "iso_fortran_env" . --include="*.f90" | grep -v "kinds.f90" | cut -d: -f1 | sort -u); do
    migrate_file "$file"
done

echo ""

# Count files after
count_after=$(grep -r "iso_fortran_env" . --include="*.f90" | grep -v "kinds.f90" | grep -v "error_unit" | wc -l)
echo "Files still using iso_fortran_env: $count_after"

# Show remaining uses (should only be error_unit)
remaining=$(grep -r "iso_fortran_env" . --include="*.f90" | grep -v "kinds.f90")
if [ ! -z "$remaining" ]; then
    echo ""
    echo "Remaining iso_fortran_env uses (these are OK if they're for error_unit):"
    echo "$remaining" | head -10
fi

echo ""
echo "✅ Migration complete!"
echo ""
echo "Files migrated: $((count_before - count_after))"