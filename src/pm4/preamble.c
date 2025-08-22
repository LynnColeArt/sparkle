// PM4 Preamble Setup - Consolidated from various test files
// This contains the standard initialization sequence for compute dispatches

#include <stdint.h>
#include <string.h>
#include "../core/pm4_constants.h"

// PM4 packet types
#define PM4_CLEAR_STATE         0x12
#define PM4_CONTEXT_CONTROL     0x28
#define PM4_SET_SH_REG          0x76
#define PM4_ACQUIRE_MEM         0x58
#define PM4_DISPATCH_DIRECT     0x15

// Register offsets
#define COMPUTE_PGM_LO               0x204
#define COMPUTE_PGM_HI               0x205
#define COMPUTE_PGM_RSRC1            0x206
#define COMPUTE_PGM_RSRC2            0x207
#define COMPUTE_NUM_THREAD_X         0x20A
#define COMPUTE_NUM_THREAD_Y         0x20B
#define COMPUTE_NUM_THREAD_Z         0x20C
#define COMPUTE_STATIC_THREAD_MGMT_SE0 0x211

// Build standard compute preamble
// Returns number of dwords written
uint32_t sp_build_compute_preamble(uint32_t* ib, uint32_t max_dwords) {
    uint32_t idx = 0;
    
    // CLEAR_STATE - reset GPU state
    ib[idx++] = (3 << 30) | PM4_CLEAR_STATE;
    ib[idx++] = 0;
    
    // CONTEXT_CONTROL - enable CS register loads
    // Using libdrm's value which we discovered works
    ib[idx++] = (3 << 30) | (1 << 16) | PM4_CONTEXT_CONTROL;
    ib[idx++] = 0x80000000;
    ib[idx++] = 0x80000000;
    
    // Enable all compute units
    ib[idx++] = (3 << 30) | (4 << 16) | PM4_SET_SH_REG;
    ib[idx++] = COMPUTE_STATIC_THREAD_MGMT_SE0;
    ib[idx++] = 0xFFFFFFFF;  // SE0
    ib[idx++] = 0xFFFFFFFF;  // SE1
    ib[idx++] = 0xFFFFFFFF;  // SE2
    ib[idx++] = 0xFFFFFFFF;  // SE3
    
    return idx;
}

// Set shader program address (GFX10 encoding)
uint32_t sp_set_shader_address(uint32_t* ib, uint64_t shader_va) {
    uint32_t idx = 0;
    
    // SET_SH_REG for COMPUTE_PGM_LO/HI
    ib[idx++] = (3 << 30) | (2 << 16) | PM4_SET_SH_REG;
    ib[idx++] = COMPUTE_PGM_LO;
    ib[idx++] = (uint32_t)(shader_va >> 8);   // bits [39:8]
    ib[idx++] = (uint32_t)(shader_va >> 40);  // bits [47:40]
    
    return idx;
}

// Set shader resources (minimal for simple kernels)
uint32_t sp_set_shader_resources(uint32_t* ib, uint32_t vgprs, uint32_t sgprs) {
    uint32_t idx = 0;
    
    // SET_SH_REG for COMPUTE_PGM_RSRC1/2
    ib[idx++] = (3 << 30) | (2 << 16) | PM4_SET_SH_REG;
    ib[idx++] = COMPUTE_PGM_RSRC1;
    
    // RSRC1: minimal config
    // bits[5:0] = VGPRs/4-1, bits[9:6] = SGPRs/8-1
    uint32_t rsrc1 = ((vgprs / 4 - 1) & 0x3F) | (((sgprs / 8 - 1) & 0xF) << 6);
    ib[idx++] = rsrc1;
    
    // RSRC2: default for compute
    ib[idx++] = 0x00000000;
    
    return idx;
}

// Set thread group dimensions
uint32_t sp_set_thread_dimensions(uint32_t* ib, uint32_t x, uint32_t y, uint32_t z) {
    uint32_t idx = 0;
    
    // SET_SH_REG for COMPUTE_NUM_THREAD_X/Y/Z
    ib[idx++] = (3 << 30) | (3 << 16) | PM4_SET_SH_REG;
    ib[idx++] = COMPUTE_NUM_THREAD_X;
    ib[idx++] = x;
    ib[idx++] = y;
    ib[idx++] = z;
    
    return idx;
}

// Build DISPATCH_DIRECT packet
uint32_t sp_build_dispatch_direct(uint32_t* ib, uint32_t dim_x, uint32_t dim_y, uint32_t dim_z) {
    uint32_t idx = 0;
    
    // DISPATCH_DIRECT with libdrm's initiator value
    ib[idx++] = (3 << 30) | (4 << 16) | PM4_DISPATCH_DIRECT;
    ib[idx++] = dim_x;
    ib[idx++] = dim_y;
    ib[idx++] = dim_z;
    ib[idx++] = 0x00000045;  // libdrm's exact initiator value
    
    return idx;
}

// Build complete compute dispatch IB
uint32_t sp_build_compute_dispatch(uint32_t* ib, uint32_t max_dwords,
                                   uint64_t shader_va,
                                   uint32_t thread_x, uint32_t thread_y, uint32_t thread_z,
                                   uint32_t grid_x, uint32_t grid_y, uint32_t grid_z) {
    uint32_t idx = 0;
    
    // Standard preamble
    idx += sp_build_compute_preamble(ib + idx, max_dwords - idx);
    
    // Shader setup
    idx += sp_set_shader_address(ib + idx, shader_va);
    idx += sp_set_shader_resources(ib + idx, 4, 8);  // minimal VGPRs/SGPRs
    idx += sp_set_thread_dimensions(ib + idx, thread_x, thread_y, thread_z);
    
    // Dispatch
    idx += sp_build_dispatch_direct(ib + idx, grid_x, grid_y, grid_z);
    
    return idx;
}