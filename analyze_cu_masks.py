#!/usr/bin/env python3

def analyze_cu_mask(value, name):
    print(f"\n{name} = 0x{value:08x}")
    print(f"Binary: {value:032b}")
    enabled = bin(value).count('1')
    print(f"CUs enabled: {enabled}/32")
    
    # Check which CUs are disabled
    disabled = []
    for i in range(32):
        if not (value & (1 << i)):
            disabled.append(i)
    
    if disabled:
        print(f"Disabled CUs: {disabled}")

print("=== CU Mask Analysis ===")

print("\nBEFORE GFX preamble:")
analyze_cu_mask(0x2049fecf, "SE0")
analyze_cu_mask(0xffffffff, "SE1")

print("\nAFTER GFX preamble:")
analyze_cu_mask(0x2850fece, "SE0") 
analyze_cu_mask(0x3fff1b0a, "SE1")

print("\nChanges:")
print(f"SE0: {bin(0x2049fecf).count('1')} → {bin(0x2850fece).count('1')} CUs")
print(f"SE1: {bin(0xffffffff).count('1')} → {bin(0x3fff1b0a).count('1')} CUs")