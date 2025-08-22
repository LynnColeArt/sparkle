#!/usr/bin/env python3
"""
Parse kernel metadata from RGA or LLVM output
Generates Fortran constants for COMPUTE_PGM_RSRC registers
"""

import json
import re
import sys
import argparse

def parse_rga_metadata(meta_file):
    """Parse RGA meta.json file"""
    with open(meta_file, 'r') as f:
        data = json.load(f)
    
    # RGA format varies, but typically has kernel info
    if 'kernels' in data:
        kernel = data['kernels'][0]  # First kernel
    else:
        kernel = data
    
    return {
        'name': kernel.get('name', 'unknown'),
        'sgpr_count': kernel.get('sgpr_count', 0),
        'vgpr_count': kernel.get('vgpr_count', 0),
        'lds_size': kernel.get('lds_size', 0),
        'scratch_size': kernel.get('scratch_size', 0),
        'wavefront_size': kernel.get('wavefront_size', 32),
    }

def parse_llvm_notes(notes_file):
    """Parse LLVM readobj output for amdhsa.kernels"""
    with open(notes_file, 'r') as f:
        content = f.read()
    
    # Look for amdhsa.kernels section
    kernels = []
    kernel_start = content.find('amdhsa.kernels:')
    if kernel_start >= 0:
        # Parse YAML-like structure
        lines = content[kernel_start:].split('\n')
        current_kernel = {}
        
        for line in lines:
            # Look for kernel properties
            if 'sgpr_count:' in line:
                current_kernel['sgpr_count'] = int(re.findall(r'\d+', line)[0])
            elif 'vgpr_count:' in line:
                current_kernel['vgpr_count'] = int(re.findall(r'\d+', line)[0])
            elif 'group_segment_fixed_size:' in line:
                current_kernel['lds_size'] = int(re.findall(r'\d+', line)[0])
            elif 'private_segment_fixed_size:' in line:
                current_kernel['scratch_size'] = int(re.findall(r'\d+', line)[0])
            elif 'wavefront_size:' in line:
                current_kernel['wavefront_size'] = int(re.findall(r'\d+', line)[0])
            elif '.name:' in line:
                current_kernel['name'] = line.split(':')[1].strip()
    
    return current_kernel if current_kernel else None

def compute_rsrc_values(metadata):
    """
    Compute COMPUTE_PGM_RSRC1/2/3 values from metadata
    Based on LLVM AMDGPU documentation
    """
    
    # RSRC1 fields (32-bit)
    # Bits [5:0] = VGPRS (granulated: (vgpr_count-1)/4)
    # Bits [9:6] = SGPRS (granulated: (sgpr_count-1)/8)
    # Other bits for float mode, priority, etc.
    
    vgpr_granulated = max(0, (metadata['vgpr_count'] - 1) // 4)
    sgpr_granulated = max(0, (metadata['sgpr_count'] - 1) // 8)
    
    rsrc1 = (vgpr_granulated & 0x3F) | ((sgpr_granulated & 0xF) << 6)
    
    # RSRC2 fields (32-bit)
    # Bit 0 = SCRATCH_EN (1 if scratch_size > 0)
    # Bits [6:1] = USER_SGPR count
    # Bit 7 = TRAP_PRESENT
    # Bits [13:8] = LDS_SIZE (granulated: lds_size/128)
    # Bits [24:14] = Various flags
    
    scratch_en = 1 if metadata['scratch_size'] > 0 else 0
    lds_granulated = metadata['lds_size'] // 128
    
    rsrc2 = scratch_en | (lds_granulated << 8)
    rsrc2 |= 0x00000092  # Common flags: USER_SGPR + TGID_X enable
    
    # RSRC3 (GFX10+)
    # Usually 0 for simple kernels
    rsrc3 = 0
    
    return {
        'rsrc1': rsrc1,
        'rsrc2': rsrc2,
        'rsrc3': rsrc3,
        'metadata': metadata
    }

def generate_fortran_code(rsrc_values):
    """Generate Fortran code snippet"""
    meta = rsrc_values['metadata']
    
    code = f"""! Kernel: {meta['name']}
! Resource requirements from compiler
integer(i32), parameter :: KERNEL_VGPR_COUNT = {meta['vgpr_count']}
integer(i32), parameter :: KERNEL_SGPR_COUNT = {meta['sgpr_count']}
integer(i32), parameter :: KERNEL_LDS_SIZE = {meta['lds_size']}
integer(i32), parameter :: KERNEL_SCRATCH_SIZE = {meta['scratch_size']}

! Computed register values
integer(i32), parameter :: KERNEL_RSRC1 = int(z'{rsrc_values['rsrc1']:08X}', i32)
integer(i32), parameter :: KERNEL_RSRC2 = int(z'{rsrc_values['rsrc2']:08X}', i32)
integer(i32), parameter :: KERNEL_RSRC3 = int(z'{rsrc_values['rsrc3']:08X}', i32)
"""
    return code

def main():
    parser = argparse.ArgumentParser(description='Parse kernel metadata')
    parser.add_argument('file', help='meta.json or hsaco_notes.txt')
    parser.add_argument('--format', choices=['rga', 'llvm', 'auto'], 
                        default='auto', help='Input format')
    args = parser.parse_args()
    
    # Detect format
    if args.format == 'auto':
        if args.file.endswith('.json'):
            args.format = 'rga'
        else:
            args.format = 'llvm'
    
    # Parse metadata
    if args.format == 'rga':
        metadata = parse_rga_metadata(args.file)
    else:
        metadata = parse_llvm_notes(args.file)
    
    if not metadata:
        print("Error: Could not parse metadata", file=sys.stderr)
        return 1
    
    # Compute register values
    rsrc_values = compute_rsrc_values(metadata)
    
    # Output
    print(f"Kernel: {metadata['name']}")
    print(f"VGPRs: {metadata['vgpr_count']}")
    print(f"SGPRs: {metadata['sgpr_count']}")
    print(f"LDS: {metadata['lds_size']} bytes")
    print(f"Scratch: {metadata['scratch_size']} bytes")
    print(f"")
    print(f"COMPUTE_PGM_RSRC1: 0x{rsrc_values['rsrc1']:08X}")
    print(f"COMPUTE_PGM_RSRC2: 0x{rsrc_values['rsrc2']:08X}")
    print(f"COMPUTE_PGM_RSRC3: 0x{rsrc_values['rsrc3']:08X}")
    print(f"")
    print("Fortran code:")
    print(generate_fortran_code(rsrc_values))
    
    return 0

if __name__ == '__main__':
    sys.exit(main())