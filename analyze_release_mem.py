#!/usr/bin/env python3

def analyze_release_mem():
    print("=== RELEASE_MEM Packet Analysis ===\n")
    
    # Header
    header = 0xC0050049
    print(f"Header: 0x{header:08X}")
    print(f"  Type: {(header >> 30) & 0x3} (should be 3)")
    print(f"  Count: {(header >> 16) & 0x3FFF} (should be 5)")
    print(f"  Opcode: {header & 0xFF} (0x49 = RELEASE_MEM)")
    
    # Control
    control = 0x20160000
    print(f"\nControl: 0x{control:08X}")
    print(f"  Binary: {control:032b}")
    print(f"  DATA_SEL: {(control >> 29) & 0x7} (1 = 32-bit immediate)")
    print(f"  INT_SEL: {(control >> 24) & 0x7}")
    print(f"  DST_SEL: {(control >> 16) & 0x1} (0 = memory)")
    print(f"  TC_ACTION: {(control >> 17) & 0x1}")
    print(f"  TC_WB_ACTION: {(control >> 18) & 0x1}")
    print(f"  TC_INV: {(control >> 20) & 0x1}")
    
    # Address
    addr_lo = 0x00000000
    addr_hi = 0x00000008
    full_addr = (addr_hi << 32) | addr_lo
    print(f"\nAddress: 0x{full_addr:016X}")
    print(f"  This is 0x800000000 which matches our signal_va")
    
    print("\n=== Potential Issues ===")
    print("1. Control flags might need different encoding")
    print("2. Address might need alignment")
    print("3. Cache policy bits might be needed")

analyze_release_mem()

print("\n=== Alternative RELEASE_MEM encoding ===")
print("Let's try EVENT_WRITE_EOP instead:")
print("  Opcode: 0x47 (EVENT_WRITE_EOP)")
print("  EVENT_TYPE: 0x2F (CACHE_FLUSH_AND_INV_TS_EVENT)")
print("  EVENT_INDEX: 0x5 (event_write_eop)")