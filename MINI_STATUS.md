# Status Update for Mini

## What We Know

1. **Both GPUs are working**:
   - Card0 (0000:13:00.0) - Raphael iGPU (0x164e) - renderD129
   - Card1 (0000:03:00.0) - 7900 XT (0x744c) - renderD128
   - Both show "Accelerator working: YES"
   - GFX rings available: 1 on each

2. **Fixed Issues**:
   - ✅ Packet count = 4 for WRITE_DATA
   - ✅ IB aligned to 32 dwords
   - ✅ DST_SEL = MEM (5) in control word
   - ✅ Using legacy bo_list_handle pattern
   - ✅ CS submission succeeds (no EINVAL)
   - ✅ Fence signals successfully

3. **Current Problem**:
   - Memory doesn't change (stays 0x12345678)
   - HQD pointers via UMR show 0xffffffff
   - Ring binary dumps exist but are unreadable
   - Can't find parsed ring info in debugfs

## Test Configurations Tried

### Working CS submission:
```c
// BO list creation (works)
handles[] = {ib_handle, signal_handle}
bo_list_args.in.operation = 0
bo_list_args.in.bo_number = 2
bo_list_args.in.bo_info_size = sizeof(uint32_t)
bo_list_args.in.bo_info_ptr = (uintptr_t)handles

// CS submission (works)
cs_args.in.ctx_id = 1 (valid)
cs_args.in.bo_list_handle = 1 (from BO list)
cs_args.in.num_chunks = 1
chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB
chunks[0].length_dw = 8 (sizeof ib_chunk / 4)
```

### IB VA mapping issue:
- Small sizes (128 bytes) fail with EINVAL
- Trying full page size (4096) in latest test

## Questions

1. Is the IB VA mapping size requirement different on newer kernels?
2. Are we missing some context initialization that Mesa/ROCm does?
3. Is there a better way to check ring state than the binary dumps?

## Next Steps
1. Run test_ring_state to check debugfs ring info
2. Try SDMA as control experiment
3. Check if we need special flags for IB VA mapping