#!/usr/bin/env python3
"""
Apply Mini's hardening updates to Fortran files systematically
"""

import os
import re
import sys
from pathlib import Path

def update_flop_calculations(content):
    """Replace manual FLOP calculations with safe function calls"""
    # Pattern for conv2d FLOP calculations
    pattern = r'int\(N,\s*int64\)\s*\*\s*int\(K,\s*int64\)\s*\*\s*int\(H_out,\s*int64\)\s*\*\s*int\(W_out,\s*int64\)\s*\*\s*&\s*\n\s*int\(C,\s*int64\)\s*\*\s*int\(kernel_size,\s*int64\)\s*\*\s*int\(kernel_size,\s*int64\)\s*\*\s*2_int64'
    
    replacement = '''conv2d_flops(int(N, int64), int(H_out, int64), int(W_out, int64), &
                                int(K, int64), int(C, int64), &
                                int(kernel_size, int64), int(kernel_size, int64))'''
    
    updated = re.sub(pattern, replacement, content)
    
    # Check if we need to add the import
    if 'conv2d_flops' in updated and 'use flopcount' not in updated:
        # Find where to insert the use statement
        use_match = re.search(r'(  use \w+.*\n)', updated)
        if use_match:
            insert_pos = use_match.end()
            updated = updated[:insert_pos] + '  use flopcount, only: conv2d_flops\n' + updated[insert_pos:]
    
    return updated

def update_gflops_literals(content):
    """Fix GFLOPS calculation literals"""
    # Replace 1.0e6 with proper kind
    content = re.sub(r'(\d+\.?\d*)(e6)(?!_)', r'\1\2_real32', content)
    content = re.sub(r'(\d+\.?\d*)(e9)(?!_)', r'\1\2_real64', content)
    return content

def update_timing_code(content):
    """Replace system_clock with time_utils"""
    updates_made = False
    
    # Pattern for system_clock with rate
    if 'call system_clock(' in content:
        # Check if already using time_utils
        if 'use time_utils' not in content:
            # Add import after other use statements
            use_match = re.search(r'(  use \w+.*\n)', content)
            if use_match:
                insert_pos = use_match.end()
                content = content[:insert_pos] + '  use time_utils, only: tic, toc_seconds\n' + content[insert_pos:]
                updates_made = True
        
        # Replace clock variable declarations
        content = re.sub(r'integer :: clock_start, clock_end, clock_rate',
                        'integer(int64) :: clock_start', content)
        
        # Replace system_clock calls
        content = re.sub(r'call system_clock\((\w+), clock_rate\)',
                        r'call tic(\1)', content)
        content = re.sub(r'call system_clock\((\w+)\)',
                        r'call tic(\1)', content)
        
        # Replace timing calculations
        # Pattern: real(clock_end - clock_start) * something / real(clock_rate)
        content = re.sub(r'real\((\w+) - (\w+),?\s*\w*\)\s*\*\s*(\d+\.?\d*)\s*/\s*real\(clock_rate,?\s*\w*\)',
                        r'toc_seconds(\2) * \3', content)
        
        updates_made = True
    
    return content, updates_made

def process_file(filepath):
    """Process a single Fortran file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original = content
        
        # Apply updates
        content = update_flop_calculations(content)
        content = update_gflops_literals(content)
        content, timing_updated = update_timing_code(content)
        
        if content != original:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"✅ Updated: {filepath}")
            return True
        else:
            print(f"⏭️  No changes needed: {filepath}")
            return False
    except Exception as e:
        print(f"❌ Error processing {filepath}: {e}")
        return False

def main():
    if len(sys.argv) > 1:
        # Process specific files
        files = sys.argv[1:]
    else:
        # Find all Fortran files
        files = []
        for pattern in ['src/*.f90', 'src/production/*.f90', 'examples/*.f90']:
            files.extend(Path('.').glob(pattern))
    
    updated_count = 0
    for filepath in files:
        if process_file(filepath):
            updated_count += 1
    
    print(f"\n🎉 Updated {updated_count} files with Mini's hardening!")

if __name__ == '__main__':
    main()