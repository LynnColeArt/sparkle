#!/usr/bin/env python3
import sys

# Read hexdump and look for patterns
with open('out2.txt', 'r') as f:
    lines = f.readlines()

print("=== Analyzing Ring Hexdump ===\n")

# Look for interesting patterns
for i, line in enumerate(lines[:50]):
    if len(line) < 60:
        continue
    
    # Extract hex portion
    hex_part = line[10:58].strip()
    hex_bytes = hex_part.split()
    
    # Look for PM4 packet headers (type 3 = 0xC0)
    for j, byte_pair in enumerate(hex_bytes):
        if len(byte_pair) == 8:  # 4-byte value
            val = int(byte_pair, 16)
            # Check for PKT3 header (bits 31:30 = 3)
            if (val >> 30) == 3:
                opcode = (val >> 8) & 0xFF
                count = val & 0xFF
                print(f"Line {i+1}, offset 0x{i*16 + j*4:04x}: PKT3 header = 0x{val:08x}")
                print(f"  Opcode = 0x{opcode:02x}, Count = {count}")
                
                # Known opcodes
                if opcode == 0x37:
                    print("  -> IT_WRITE_DATA")
                elif opcode == 0x10:
                    print("  -> IT_NOP")
                elif opcode == 0x47:
                    print("  -> IT_EVENT_WRITE_EOP")
                elif opcode == 0x79:
                    print("  -> IT_SET_UCONFIG_REG")
                elif opcode == 0x68:
                    print("  -> IT_SET_CONFIG_REG")
                elif opcode == 0x69:
                    print("  -> IT_SET_CONTEXT_REG")
                elif opcode == 0x76:
                    print("  -> IT_SET_SH_REG")
                    
# Look for specific values
print("\n=== Looking for test values ===")
test_values = {
    "deadbeef": "DEADBEEF marker",
    "cafebabe": "Our test value",
    "12345678": "Initial signal value",
    "a5a5a5a5": "CP_SCRATCH test value"
}

for i, line in enumerate(lines):
    if len(line) < 60:
        continue
    
    hex_part = line[10:58].replace(' ', '').lower()
    for test_val, desc in test_values.items():
        if test_val in hex_part:
            offset = line[:8]
            print(f"Found {desc} ({test_val}) at offset {offset}")
            
# Check for ring pointers
print("\n=== Checking for patterns ===")
print("Looking for WPTR/RPTR patterns...")

# The pattern 0xc3a10000 appears frequently
c3a1_count = 0
for line in lines:
    if "c3 a1" in line or "a1 c3" in line:
        c3a1_count += 1
        
print(f"Found 0xc3a1 pattern {c3a1_count} times")

# Look for our IB address pattern (0x800000000 or 0x800001000)
print("\nLooking for VA addresses...")
for i, line in enumerate(lines[:100]):
    if "00 00 00 08" in line or "00 10 00 08" in line:
        print(f"Possible VA at line {i+1}: {line.strip()}")