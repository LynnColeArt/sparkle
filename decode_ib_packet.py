#!/usr/bin/env python3

# Decode the INDIRECT_BUFFER packets we see
print("=== Decoding INDIRECT_BUFFER packets ===\n")

# Example from ring dump:
# 000050 c0023f00 00b11700 ffff8000 03000098
packet = 0xc0023f00
ib_base_lo = 0x00b11700
ib_base_hi = 0xffff8000
ib_size = 0x03000098

print(f"Packet: 0x{packet:08x}")
opcode = (packet >> 8) & 0xFF
count = packet & 0xFF
print(f"  Opcode: 0x{opcode:02x} (IT_INDIRECT_BUFFER)")
print(f"  Count: {count}")

print(f"\nIB_BASE_LO: 0x{ib_base_lo:08x}")
print(f"IB_BASE_HI: 0x{ib_base_hi:08x}")

# Reconstruct the VA
# On GFX10+, IB address is shifted by 8
ib_va = ((ib_base_hi & 0xFFFF) << 32) | ib_base_lo
ib_va = ib_va << 8
print(f"Reconstructed IB VA: 0x{ib_va:016x}")

print(f"\nIB_SIZE: 0x{ib_size:08x}")
ib_bytes = ib_size & 0xFFFFF  # Lower 20 bits
print(f"  Size in bytes: {ib_bytes} (0x{ib_bytes:x})")
print(f"  Size in dwords: {ib_bytes // 4}")

print("\n--- Our IB for comparison ---")
our_va = 0x800004000
our_shifted = our_va >> 8
print(f"Our IB VA: 0x{our_va:016x}")
print(f"Our IB shifted >> 8: 0x{our_shifted:08x}")
print(f"Expected IB_BASE_LO: 0x{our_shifted & 0xFFFFFFFF:08x}")
print(f"Expected IB_BASE_HI: 0x{(our_shifted >> 32) & 0xFFFF:08x}")

print("\nConclusion: The kernel is scheduling other IBs, not ours!")