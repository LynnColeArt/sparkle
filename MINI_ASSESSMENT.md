# Assessment for Mini: PM4 GFX Ring Execution Status

## Current Situation
We're trying to get even the simplest GFX ring packet (WRITE_DATA) to execute on Raphael iGPU (0x164e). Despite following all of Mini's fixes, packets still don't execute.

## What We've Tried

### 1. Packet Structure Fixes ✅
- Fixed WRITE_DATA header count: `0xC0003704` (count=4 for 4 payload dwords)
- Fixed IB alignment: Padded to 32 dwords with NOP
- Fixed control word: Used minimal `0x00010000` (only WR_ONE_ADDR=1)

### 2. Submission Methods Tested

#### A. BO List Only (test_bo_list_only.c)
```c
// Single IB chunk, bo_list_handle set
cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
cs_args.in.num_chunks = 1;  // Only IB chunk
```
Result: ✅ Submits, ✅ Fence signals, ❌ No execution

#### B. Two Chunks (test_two_chunks.c)
```c
// IB chunk + BO_HANDLES chunk
chunks[1].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
chunks[1].length_dw = 2;
chunks[1].chunk_data = (uintptr_t)handles;
```
Result: ❌ CS submission fails with EINVAL

#### C. BO List + BO_HANDLES (hybrid)
```c
// Both bo_list_handle AND BO_HANDLES chunk
cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
cs_args.in.num_chunks = 2;  // IB + BO_HANDLES
```
Result: ❌ CS submission fails with EINVAL

## Key Observations

1. **Fence Always Signals**: Even with no execution, the fence completes successfully
2. **No Memory Write**: Signal buffer stays at initial value (0x12345678)
3. **HQD Pointers**: Unable to check without sudo, but critical for diagnosis
4. **Ring Available**: simple_ring_test shows GFX ring available with proper alignment
5. **CU Status**: SE0 shows partial CUs enabled (0x2049fecf instead of 0xffffffff)

## Exact Packet Being Submitted
```
DW[0] = 0xC0003704  // PKT3 | (IT_WRITE_DATA << 8) | 4
DW[1] = 0x00010000  // WR_ONE_ADDR=1, rest zero
DW[2] = 0x00000000  // VA low (0x800000000)
DW[3] = 0x00000008  // VA high
DW[4] = 0xCAFEBABE  // Data to write
DW[5] = 0xC0001019  // PKT3 | (IT_NOP << 8) | 25
DW[6-31] = 0x00000000  // NOP padding
```

## Critical Questions for Mini

1. **BO_HANDLES Chunk**: When using bo_list_handle, should we ALSO include BO_HANDLES chunk? The kernel seems to reject having both.

2. **Missing Initialization**: Are we missing some critical GFX ring initialization that Mesa/ROCm normally does? The fence signals but no execution suggests the ring might be in a special state.

3. **HQD Pointer Check**: Without sudo, we can't check CP_HQD_PQ_RPTR/WPTR. Is there another way to verify ring consumption?

4. **Control Word**: We tried minimal 0x00010000. Should we try other combinations? Some tests use 0x40010000.

5. **Ring State**: Is there a way to check if the GFX ring is in "direct submit" mode vs some other mode that requires additional setup?

## Next Debug Steps

1. Get HQD pointer readings (need sudo access)
2. Try CP_SCRATCH write (no memory involved)
3. Compare with working libdrm test trace
4. Check if kernel driver expects specific initialization sequence

## Hypothesis
The most likely issue is that we're missing some userspace driver initialization that the kernel expects. The fact that the fence signals but no execution occurs suggests the submission is accepted but the ring isn't actually processing our IB.