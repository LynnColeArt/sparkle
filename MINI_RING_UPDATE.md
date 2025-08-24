# Ring Update for Mini

## What We Fixed
✅ 4KB page-aligned IB mapping (no more EINVAL!)
✅ Correct ring file (dri/1 for iGPU, not dri/0)
✅ Legacy bo_list pattern
✅ DST_SEL=MEM (0x40010500)

## What We See
1. **Ring IS active** - Counter changes from 0x00000100 to 0x00000500
2. **Submission succeeds** - No errors, fence signals
3. **Ring contains packets** - Multiple PM4 packets visible:
   - 0xC0032200 (unknown opcode 0x22)
   - 0xC0053C00 (opcode 0x3C)
   - 0xC0064900 (opcode 0x49)
   - 0xC0012800 (opcode 0x28)

4. **BUT: No sign of our packet**:
   - No 0xC0003704 (WRITE_DATA)
   - No 0xCAFEBABE (our data)
   - No 0x40010500 (our control word)

## Hypothesis
Either:
1. Our IB is not being executed (CS wiring still wrong?)
2. The ring wrapped and our packet was overwritten
3. We're submitting to a different ring than we're dumping

## Key Question
The ring shows activity but not our specific packet. Are we missing something in the CS submission that prevents our IB from being scheduled?

## Debug Data
- IB VA: 0x800004000 (4KB aligned)
- Signal VA: 0x800000000 (4KB aligned)
- Both BOs in GTT domain
- BO list contains both handles
- CS submission succeeds with seq=1

What else could prevent the CP from fetching our IB?