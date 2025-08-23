# PM4 GPU Wiring Status

## What We've Completed

### ✅ Mini's Reorganization (Phase 1)
- Created unified submit API: `sp_submit_ib_with_bos`
- Moved preamble setup to `pm4/preamble.c`
- Created `sporkle pm4 --selftest` command
- Cleaned up duplicate submit helpers
- Moved tests from `/src` to `/tests`

### ✅ QA Findings
- Removed mock GPU OpenGL implementation
- Fixed hardcoded shader address in `pm4_safe_submit.f90`
- Identified real implementations that exist:
  - PM4 compute (`sporkle_pm4_compute.f90`)
  - RDNA3 ISA shaders (`sporkle_rdna3_shaders.f90`)
  - AMDGPU direct interface (`sporkle_amdgpu_direct.f90`)

## What Still Needs Wiring

### 🔧 Production Code Updates Needed

1. **Memory Allocation** (`sporkle_memory.f90`)
   - Currently: Returns placeholder with print statement
   - Needs: Call device's allocate method or use AMDGPU direct

2. **GPU Dispatch** (`sporkle_gpu_dispatch.f90`)
   - Currently: Uses fake GPU pointers `int(loc(mem), int64)`
   - Needs: Use real GPU VA from AMDGPU allocation

3. **Shader Cache** (`gpu_dynamic_shader_cache.f90`)
   - Currently: Returns "// Dynamic shader placeholder"
   - Needs: Either generate real shaders or use pre-compiled ISA

4. **Device Integration**
   - Currently: CPU/AMD devices use host memory
   - Needs: Wire to AMDGPU buffer allocation

## Real Implementations Available

### PM4 Path (Ready to Use)
```fortran
! From sporkle_pm4_compute.f90
shader_addr = pm4_compile_shader("shader_name", "")
status = pm4_execute_compute(ctx, shader_addr, buffers, wx, wy, wz)
```

### AMDGPU Direct (Ready to Use)
```fortran
! From sporkle_amdgpu_direct.f90
buffer = amdgpu_allocate_buffer(device, size, AMDGPU_GEM_DOMAIN_GTT)
status = amdgpu_map_buffer(device, buffer)
status = amdgpu_map_va(device, buffer, va_addr)
```

### ISA Shaders (Available)
- `get_simple_copy_shader()` - Working RDNA3 copy shader
- `get_vector_add_shader()` - Working RDNA3 vector add
- Missing: `get_conv2d_shader()` - Needs to be added

## Next Steps

1. **Option A: Update existing production code**
   - Modify `sporkle_memory.f90` to use device allocate
   - Update `sporkle_gpu_dispatch.f90` to use real VAs
   - Fix shader placeholders

2. **Option B: Create new PM4-based production path**
   - `sporkle_gpu_dispatch_pm4.f90` (started)
   - `amdgpu_device.f90` (created)
   - Direct PM4 execution path

3. **Option C: Focus on getting waves to launch**
   - Debug why PM4 compute isn't launching waves
   - Add missing initialization/mode settings
   - Verify shader upload and execution

## The Real Issue

The placeholders exist because the production code predates the PM4 implementation. We have all the pieces - they just need to be connected properly.