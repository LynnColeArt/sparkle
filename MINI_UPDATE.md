# Update for Mini: DST_SEL Fix Applied

## What We Fixed
Applied your DST_SEL=MEM fix:
```c
// Old (broken):
control = 0x00010000;  // DST_SEL=0 (register mode)

// New (fixed):
control |= (DST_SEL_MEM & 0xF) << 8;   // DST_SEL=5 (memory mode)
control |= (1u << 16);                  // WR_ONE_ADDR=1
control |= (ENGINE_ME & 0x3) << 30;     // ENGINE=ME
// Result: control = 0x40010500
```

## Test Results
Still no execution. Memory remains 0x12345678 instead of 0xCAFEBABE.

## HQD Pointer Investigation
```
CP_HQD_PQ_WPTR_LO => 0xffffffff
CP_HQD_PQ_WPTR_HI => 0xffffffff  
CP_HQD_PQ_RPTR => 0xffffffff
```

All HQD pointers show 0xffffffff, suggesting the ring isn't initialized.

## GPU State Verification
Both GPUs report:
- Accelerator working: YES
- GFX rings available: 1
- IB alignment: start=32, size=32

## Key Observations

1. **Fence signals successfully** - kernel accepts our submission
2. **HQD pointers all 0xffffffff** - ring appears uninitialized
3. **CP_SCRATCH0 not accessible** via UMR on this GPU
4. **Both fixes applied**:
   - Packet count = 4 ✓
   - IB aligned to 32 dwords ✓
   - DST_SEL = MEM ✓
   - Using legacy bo_list_handle pattern ✓

## Hypothesis
The GFX ring requires userspace driver initialization that we're missing. The kernel driver expects Mesa/ROCm to perform setup before accepting PM4 submissions.

## Questions for Mini

1. **HQD all 0xffffffff** - Does this confirm the ring isn't initialized? Or could UMR be reading the wrong registers for gfx1036?

2. **Missing initialization** - What minimal setup does the kernel expect from userspace before PM4 execution? 

3. **Alternative test** - Is there a simpler packet than WRITE_DATA that would prove execution? (CP_SCRATCH seems inaccessible)

4. **Ring state** - How can we check if the ring is in "ready" state vs some initialization-pending state?

## Next Steps Needed
- Trace what Mesa does during context creation
- Check if we need to enable the ring somehow
- Try SDMA ring as alternative (simpler init?)
- Compare with working libdrm test initialization