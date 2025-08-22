#!/usr/bin/env python3
"""
Batch update test files to use unified submit API
"""
import os
import re
import sys

def update_submit_calls(content):
    """Update submit function calls to use unified API"""
    modified = False
    
    # Pattern 1: sp_submit_ib(ctx, ib_bo, size, fence)
    # Replace with: sp_submit_ib_with_bos(ctx, ib_bo, size, [c_ptr::], fence)
    pattern1 = r'(\s*)status\s*=\s*sp_submit_ib\(([^,]+),\s*([^,]+),\s*([^,]+),\s*([^)]+)\)'
    replacement1 = r'\1status = sp_submit_ib_with_bos(\2, \3, \4, [c_ptr::], \5)'
    
    new_content, count1 = re.subn(pattern1, replacement1, content)
    if count1 > 0:
        content = new_content
        modified = True
        print(f"  Updated {count1} sp_submit_ib calls")
    
    # Pattern 2: sp_submit_ib_with_bo(ctx, ib_bo, size, data_bo, fence)
    # Replace with: sp_submit_ib_with_bos(ctx, ib_bo, size, [data_bo_ptr], fence)
    pattern2 = r'(\s*)status\s*=\s*sp_submit_ib_with_bo\(([^,]+),\s*([^,]+),\s*([^,]+),\s*([^,]+),\s*([^)]+)\)'
    
    def replace_with_bo(match):
        indent = match.group(1)
        ctx = match.group(2)
        ib_bo = match.group(3)
        size = match.group(4)
        data_bo = match.group(5).strip()
        fence = match.group(6)
        
        # Check if data_bo ends with _ptr
        if data_bo.endswith('_ptr'):
            return f'{indent}status = sp_submit_ib_with_bos({ctx}, {ib_bo}, {size}, [{data_bo}], {fence})'
        else:
            # Need to add _ptr suffix
            return f'{indent}status = sp_submit_ib_with_bos({ctx}, {ib_bo}, {size}, [{data_bo}_ptr], {fence})'
    
    new_content, count2 = re.subn(pattern2, replace_with_bo, content)
    if count2 > 0:
        content = new_content
        modified = True
        print(f"  Updated {count2} sp_submit_ib_with_bo calls")
    
    return content, modified

def process_file(filepath):
    """Process a single test file"""
    print(f"\nProcessing: {filepath}")
    
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Skip if already using unified API
    if 'sp_submit_ib_with_bos' in content:
        print("  Already uses unified API, skipping")
        return False
    
    # Skip if using sp_submit_ib_ring (special debug function)
    if 'sp_submit_ib_ring' in content and 'sp_submit_ib(' not in content.replace('sp_submit_ib_ring', ''):
        print("  Uses sp_submit_ib_ring only, skipping")
        return False
    
    new_content, modified = update_submit_calls(content)
    
    if modified:
        with open(filepath, 'w') as f:
            f.write(new_content)
        print("  ✓ Updated successfully")
        return True
    else:
        print("  No changes needed")
        return False

def main():
    """Main function"""
    test_dir = 'tests'
    updated_count = 0
    
    # Get all test files
    test_files = [f for f in os.listdir(test_dir) if f.endswith('.f90') and f.startswith('test_pm4_')]
    test_files.sort()
    
    print(f"Found {len(test_files)} PM4 test files")
    
    for test_file in test_files:
        filepath = os.path.join(test_dir, test_file)
        if process_file(filepath):
            updated_count += 1
    
    print(f"\n{'='*50}")
    print(f"Summary: Updated {updated_count} files")
    
    if updated_count > 0:
        print("\nIMPORTANT: Remember to rebuild tests after these changes!")

if __name__ == '__main__':
    main()