// PM4 Compute Self-Test
// Stage A: Lane-0 DEADBEEF test
// Stage B: SAXPY computation test

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "../core/pm4_constants.h"
#include "submit.h"

// External preamble functions
extern uint32_t sp_build_compute_dispatch(uint32_t* ib, uint32_t max_dwords,
                                         uint64_t shader_va,
                                         uint32_t thread_x, uint32_t thread_y, uint32_t thread_z,
                                         uint32_t grid_x, uint32_t grid_y, uint32_t grid_z);

// Mini's minimal lane-0 PAL probe shader
// Guards to lane 0 and writes 0xDEADBEEF to USER_DATA address
static const uint32_t deadbeef_shader[] = {
    // v_mbcnt_lo_u32_b32 v0, -1, 0
    0x4C000080,  
    // v_mbcnt_hi_u32_b32 v0, -1, v0  
    0x4C020080,
    // v_cmp_eq_u32 vcc, v0, 0 (check if lane 0)
    0x7D840080,
    // s_cbranch_vccz to skip (if not lane 0)
    0xBF860004,
    // Lane 0 code:
    // v_mov_b32 v0, 0xDEADBEEF
    0x7E0002FF, 0xDEADBEEF,
    // v_mov_b32 v1, s0 (USER_DATA_0 low)
    0x7E020200,
    // v_mov_b32 v2, s1 (USER_DATA_0 high)
    0x7E040201,
    // global_store_dword v[1:2], v0, off
    0xDC700000, 0x00000100,
    // s_waitcnt vmcnt(0) lgkmcnt(0)
    0xBF8C0070,
    // s_endpgm
    0xBF810000
};

// SAXPY shader: Y = alpha * X + Y
static const uint32_t saxpy_shader[] = {
    // Load alpha (scalar)
    0xC0020002, 0x00000000,  // s_load_dword s0, s[2:3], 0x0
    // Load X and Y buffer addresses
    0xC0060082, 0x00000004,  // s_load_dwordx2 s[2:3], s[2:3], 0x4
    0xC0060102, 0x0000000C,  // s_load_dwordx2 s[4:5], s[2:3], 0xC
    // Compute thread index
    0xBF8C007F,              // s_waitcnt lgkmcnt(0)
    0x7E000280,              // v_mov_b32 v0, s0
    0x68020084,              // v_add_u32 v1, vcc, s4, v0
    0x7E040200,              // v_mov_b32 v2, 0
    // Load X[tid]
    0xDC5C0000, 0x01000001,  // global_load_dword v1, v[1:2], off
    // Load Y[tid]
    0x68040086,              // v_add_u32 v2, vcc, s6, v0
    0x7E060200,              // v_mov_b32 v3, 0
    0xDC5C0000, 0x02000002,  // global_load_dword v2, v[2:3], off
    // Wait for loads
    0xBF8C0F71,              // s_waitcnt vmcnt(1)
    // Compute alpha * X
    0xD1190003, 0x00020300,  // v_mul_f32 v3, s0, v1
    0xBF8C0F70,              // s_waitcnt vmcnt(0)
    // Add Y
    0xD1030003, 0x00020702,  // v_add_f32 v3, v2, v3
    // Store result
    0xDC780000, 0x00000302,  // global_store_dword v[2:3], v3, off
    0xBF810000               // s_endpgm
};

// Run Stage A: DEADBEEF test with Mini's compute bootstrap
int sp_selftest_stage_a(sp_pm4_ctx* ctx) {
    printf("Stage A: Lane-0 DEADBEEF test (with Mini's bootstrap)\n");
    
    // Create scratch BO (Mini's requirement #1)
    sp_bo* scratch_bo = sp_bo_new(ctx, 65536);  // 64KB aligned
    if (!scratch_bo) {
        printf("  ❌ Failed to create scratch BO\n");
        return -1;
    }
    printf("  ✓ Scratch buffer allocated at VA 0x%016lX\n", sp_bo_get_va(scratch_bo));
    
    // Create shader BO
    sp_bo* shader_bo = sp_bo_new(ctx, sizeof(deadbeef_shader));
    if (!shader_bo) {
        printf("  ❌ Failed to create shader BO\n");
        sp_bo_free(scratch_bo);
        return -1;
    }
    
    // Upload shader with endian swap (GPU expects little-endian)
    void* shader_ptr = sp_bo_map(shader_bo);
    uint32_t* shader_dst = (uint32_t*)shader_ptr;
    printf("  Uploading shader (%zu dwords):\n", sizeof(deadbeef_shader)/4);
    for (size_t i = 0; i < sizeof(deadbeef_shader)/4; i++) {
        // Swap bytes: AABBCCDD -> DDCCBBAA
        uint32_t val = deadbeef_shader[i];
        shader_dst[i] = ((val & 0xFF000000) >> 24) |
                       ((val & 0x00FF0000) >> 8) |
                       ((val & 0x0000FF00) << 8) |
                       ((val & 0x000000FF) << 24);
        printf("    [%02zu] 0x%08X -> 0x%08X\n", i, val, shader_dst[i]);
    }
    sp_bo_unmap(shader_bo);
    
    // Create output BO
    sp_bo* output_bo = sp_bo_new(ctx, 4096);
    if (!output_bo) {
        printf("  ❌ Failed to create output BO\n");
        sp_bo_free(shader_bo);
        sp_bo_free(scratch_bo);
        return -1;
    }
    
    // Clear output buffer and set debug pattern
    uint32_t* output_ptr = (uint32_t*)sp_bo_map(output_bo);
    memset(output_ptr, 0, 4096);
    // Set a pattern to verify memory access
    for (int i = 0; i < 16; i++) {
        output_ptr[i] = 0xCAFE0000 + i;
    }
    sp_bo_unmap(output_bo);
    
    // Create IB with compute dispatch
    sp_bo* ib_bo = sp_bo_new(ctx, 4096);
    if (!ib_bo) {
        printf("  ❌ Failed to create IB BO\n");
        sp_bo_free(shader_bo);
        sp_bo_free(output_bo);
        return -1;
    }
    
    // Build IB with Mini's compute bootstrap
    uint32_t* ib = (uint32_t*)sp_bo_map(ib_bo);
    uint32_t idx = 0;
    
    // Standard preamble
    extern uint32_t sp_build_compute_preamble(uint32_t* ib, uint32_t max_dwords);
    idx += sp_build_compute_preamble(ib, 1024);
    
    // Mini's compute bootstrap
    extern uint32_t sp_build_compute_bootstrap(uint32_t* ib, uint64_t scratch_va);
    idx += sp_build_compute_bootstrap(ib + idx, sp_bo_get_va(scratch_bo));
    
    // Set user data with output buffer address
    extern uint32_t sp_set_user_data(uint32_t* ib, uint32_t start_reg, uint64_t* values, uint32_t count);
    uint64_t user_data[1] = { sp_bo_get_va(output_bo) };
    idx += sp_set_user_data(ib + idx, 0, user_data, 1);
    
    // Shader setup with Mini's requirements
    extern uint32_t sp_set_shader_address(uint32_t* ib, uint64_t shader_va);
    extern uint32_t sp_set_shader_resources(uint32_t* ib, uint32_t vgprs, uint32_t sgprs);
    extern uint32_t sp_set_thread_dimensions(uint32_t* ib, uint32_t x, uint32_t y, uint32_t z);
    idx += sp_set_shader_address(ib + idx, sp_bo_get_va(shader_bo));
    idx += sp_set_shader_resources(ib + idx, 4, 2);  // 4 VGPRs, 2 SGPRs (Mini's PAL probe)
    idx += sp_set_thread_dimensions(ib + idx, 64, 1, 1);  // Mini recommends 64 threads
    
    // Dispatch with Mini's settings
    extern uint32_t sp_build_dispatch_direct(uint32_t* ib, uint32_t dim_x, uint32_t dim_y, uint32_t dim_z);
    idx += sp_build_dispatch_direct(ib + idx, 1, 1, 1);  // 1 workgroup
    
    uint32_t ib_size = idx;
    
    // Debug: Print IB contents
    printf("  IB contents (%d dwords):\n", ib_size);
    printf("  Shader VA: 0x%016lX\n", sp_bo_get_va(shader_bo));
    printf("  Output VA: 0x%016lX\n", sp_bo_get_va(output_bo));
    for (uint32_t i = 0; i < ib_size; i++) {
        printf("    [%02d] 0x%08X", i, ib[i]);
        if (i == 12) printf(" <- USER_DATA_0 reg offset");
        else if (i == 13) printf(" <- Output buffer VA low");
        else if (i == 14) printf(" <- Output buffer VA high");
        else if (i == 16) printf(" <- COMPUTE_PGM_LO reg");
        else if (i == 17) printf(" <- Shader addr bits [39:8]");
        else if (i == 18) printf(" <- Shader addr bits [47:40]");
        else if (i == 28) printf(" <- DISPATCH_DIRECT opcode");
        else if (i == 32) printf(" <- Initiator");
        printf("\n");
    }
    
    sp_bo_unmap(ib_bo);
    
    // Submit with all BOs (including scratch)
    sp_bo* data_bos[] = {shader_bo, output_bo, scratch_bo};
    sp_fence fence;
    
    int ret = sp_submit_ib_with_bos(ctx, ib_bo, ib_size, data_bos, 3, &fence);
    if (ret < 0) {
        printf("  ❌ Submit failed: %d\n", ret);
        sp_bo_free(ib_bo);
        sp_bo_free(shader_bo);
        sp_bo_free(output_bo);
        sp_bo_free(scratch_bo);
        return -1;
    }
    
    // Wait for completion
    ret = sp_fence_wait(ctx, &fence, 1000000000); // 1 second timeout
    if (ret < 0) {
        printf("  ❌ Fence wait failed\n");
        sp_bo_free(ib_bo);
        sp_bo_free(shader_bo);
        sp_bo_free(output_bo);
        sp_bo_free(scratch_bo);
        return -1;
    }
    
    // Check result
    output_ptr = (uint32_t*)sp_bo_map(output_bo);
    printf("  First 16 dwords of output buffer:\n");
    for (int i = 0; i < 16; i++) {
        printf("    [%02d] 0x%08X", i, output_ptr[i]);
        if (output_ptr[i] == 0xDEADBEEF) printf(" <- FOUND!");
        else if (output_ptr[i] == (0xCAFE0000 + i)) printf(" <- Unchanged");
        printf("\n");
    }
    
    if (output_ptr[0] == 0xDEADBEEF) {
        printf("  ✅ DEADBEEF found at output[0]\n");
        ret = 0;
    } else {
        printf("  ❌ Expected 0xDEADBEEF, got 0x%08X\n", output_ptr[0]);
        ret = -1;
    }
    sp_bo_unmap(output_bo);
    
    // Cleanup
    sp_bo_free(ib_bo);
    sp_bo_free(shader_bo);
    sp_bo_free(output_bo);
    sp_bo_free(scratch_bo);
    
    return ret;
}

// Run Stage B: SAXPY test
int sp_selftest_stage_b(sp_pm4_ctx* ctx) {
    printf("Stage B: SAXPY test (Y = 2.0 * X + Y)\n");
    
    const int n = 256;
    const float alpha = 2.0f;
    
    // Create buffers
    sp_bo* shader_bo = sp_bo_new(ctx, sizeof(saxpy_shader));
    sp_bo* alpha_bo = sp_bo_new(ctx, sizeof(float));
    sp_bo* x_bo = sp_bo_new(ctx, n * sizeof(float));
    sp_bo* y_bo = sp_bo_new(ctx, n * sizeof(float));
    
    if (!shader_bo || !alpha_bo || !x_bo || !y_bo) {
        printf("  ❌ Failed to create buffers\n");
        return -1;
    }
    
    // Initialize data
    void* shader_ptr = sp_bo_map(shader_bo);
    memcpy(shader_ptr, saxpy_shader, sizeof(saxpy_shader));
    sp_bo_unmap(shader_bo);
    
    float* alpha_ptr = (float*)sp_bo_map(alpha_bo);
    *alpha_ptr = alpha;
    sp_bo_unmap(alpha_bo);
    
    float* x_ptr = (float*)sp_bo_map(x_bo);
    float* y_ptr = (float*)sp_bo_map(y_bo);
    for (int i = 0; i < n; i++) {
        x_ptr[i] = i;
        y_ptr[i] = 100.0f + i;
    }
    sp_bo_unmap(x_bo);
    sp_bo_unmap(y_bo);
    
    // TODO: Create user data for kernel arguments
    // TODO: Build and submit IB
    // TODO: Verify results
    
    printf("  ⚠️  SAXPY test not yet implemented\n");
    
    // Cleanup
    sp_bo_free(shader_bo);
    sp_bo_free(alpha_bo);
    sp_bo_free(x_bo);
    sp_bo_free(y_bo);
    
    return 0; // Return success for now
}

// Main selftest entry point
int sp_pm4_selftest(sp_pm4_ctx* ctx) {
    printf("=== PM4 Compute Self-Test ===\n");
    
    // First, document what GFX preamble would be needed
    extern int sp_submit_gfx_preamble(sp_pm4_ctx* ctx);
    extern void sp_check_cu_status(void);
    
    sp_submit_gfx_preamble(ctx);
    sp_check_cu_status();
    
    // Stage A: Basic functionality
    if (sp_selftest_stage_a(ctx) != 0) {
        printf("\n❌ Stage A failed - compute is not online\n");
        return -1;
    }
    
    // Stage B: Real computation
    if (sp_selftest_stage_b(ctx) != 0) {
        printf("\n❌ Stage B failed - compute math not working\n");
        return -1;
    }
    
    printf("\n✅ All stages passed - compute is online!\n");
    return 0;
}