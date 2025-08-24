// PM4 Preamble Setup - Consolidated from various test files
// This contains the standard initialization sequence for compute dispatches

#include <stdint.h>
#include <string.h>
#include "../core/pm4_constants.h"

// PM4 packet types
#define PM4_CLEAR_STATE         0x12
#define PM4_CONTEXT_CONTROL     0x28
#define PM4_SET_SH_REG          0x76
#define PM4_SET_SH_REG_INDEX    0x9B
#define PM4_SET_UCONFIG_REG     0x79
#define PM4_ACQUIRE_MEM         0x58
#define PM4_DISPATCH_DIRECT     0x15

// SET_SH_REG_INDEX flags
#define SH_REG_INDEX_SE_BROADCAST   (1 << 31)
#define SH_REG_INDEX_SH_BROADCAST   (1 << 30)

// Register offsets
#define COMPUTE_PGM_LO               0x204
#define COMPUTE_PGM_HI               0x205
#define COMPUTE_PGM_RSRC1            0x206
#define COMPUTE_PGM_RSRC2            0x207
#define COMPUTE_NUM_THREAD_X         0x20A
#define COMPUTE_NUM_THREAD_Y         0x20B
#define COMPUTE_NUM_THREAD_Z         0x20C
#define COMPUTE_STATIC_THREAD_MGMT_SE0 0x211
#define COMPUTE_USER_DATA_0          0x240
#define COMPUTE_TMPRING_SIZE         0x218
#define SH_MEM_CONFIG                0x217
#define SH_MEM_BASES                 0x21B

// Mini's compute bootstrap implementation
uint32_t sp_build_compute_bootstrap(uint32_t* ib, uint64_t scratch_va) {
    uint32_t idx = 0;
    
    // 1. Set up scratch ring (even if not used) - use indexed write with broadcast
    ib[idx++] = (3 << 30) | (1 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = COMPUTE_TMPRING_SIZE | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    ib[idx++] = 16;  // 16 granules (~64KB)
    
    ib[idx++] = (3 << 30) | (2 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = COMPUTE_START_X | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;  // 0x219 - using as TMPRING_BASE_LO
    ib[idx++] = (uint32_t)(scratch_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)(scratch_va >> 32);
    
    // 2. Set up SH_MEM_CONFIG for 64-bit flat addressing - use indexed write
    ib[idx++] = (3 << 30) | (1 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = SH_MEM_CONFIG | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    ib[idx++] = 0x00000000;  // 64-bit flat, aligned mode, no GDS
    
    // 3. Set SH_MEM_BASES to sane values - use indexed write
    ib[idx++] = (3 << 30) | (1 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = SH_MEM_BASES | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    ib[idx++] = 0x00000000;  // All bases at 0, bounds at max
    
    return idx;
}

// Build standard compute preamble with Mini's bootstraps
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
    
    // NOTE: We can't set COMPUTE_STATIC_THREAD_MGMT from compute ring
    // as it's a UCONFIG register. The kernel driver should have already
    // enabled CUs, or we need to submit a GFX IB to do it.
    
    return idx;
}

// Set shader program address (GFX10 encoding)
uint32_t sp_set_shader_address(uint32_t* ib, uint64_t shader_va) {
    uint32_t idx = 0;
    
    // SET_SH_REG_INDEX for COMPUTE_PGM_LO/HI with broadcast
    ib[idx++] = (3 << 30) | (2 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = COMPUTE_PGM_LO | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    ib[idx++] = (uint32_t)(shader_va >> 8);   // bits [39:8]
    ib[idx++] = (uint32_t)(shader_va >> 40);  // bits [47:40]
    
    return idx;
}

// Set shader resources following Mini's encoded rules
uint32_t sp_set_shader_resources(uint32_t* ib, uint32_t vgprs, uint32_t sgprs) {
    uint32_t idx = 0;
    
    // SET_SH_REG_INDEX for COMPUTE_PGM_RSRC1/2 with broadcast
    ib[idx++] = (3 << 30) | (2 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = COMPUTE_PGM_RSRC1 | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    
    // RSRC1: Mini's encoded format
    // bits[5:0] = ceil(VGPRs/4)-1, bits[9:6] = ceil(SGPRs/8)-1
    uint32_t vgpr_granules = (vgprs + 3) / 4;  // ceil
    uint32_t sgpr_granules = (sgprs + 7) / 8;  // ceil
    uint32_t rsrc1 = ((vgpr_granules - 1) & 0x3F) | (((sgpr_granules - 1) & 0xF) << 6);
    // Add other required bits
    rsrc1 |= (1 << 18);  // DX10_CLAMP
    rsrc1 |= (1 << 20);  // IEEE_MODE
    ib[idx++] = rsrc1;
    
    // RSRC2: Mini's settings
    uint32_t rsrc2 = 0;
    rsrc2 |= (2 << 1);   // USER_SGPR_COUNT = 2 (for USER_DATA_0/1)
    rsrc2 |= (0 << 6);   // SCRATCH_EN = 0
    rsrc2 |= (1 << 7);   // USER_SGPR = 1
    rsrc2 |= (0 << 8);   // TRAP_PRESENT = 0
    rsrc2 |= (1 << 9);   // TGID_X_EN = 1
    rsrc2 |= (0 << 10);  // TGID_Y_EN = 0
    rsrc2 |= (0 << 11);  // TGID_Z_EN = 0
    rsrc2 |= (0 << 12);  // TG_SIZE_EN = 0
    rsrc2 |= (0 << 15);  // LDS_SIZE = 0
    ib[idx++] = rsrc2;
    
    return idx;
}

// Set thread group dimensions and start positions
uint32_t sp_set_thread_dimensions(uint32_t* ib, uint32_t x, uint32_t y, uint32_t z) {
    uint32_t idx = 0;
    
    // SET_SH_REG_INDEX for COMPUTE_NUM_THREAD_X/Y/Z with broadcast
    ib[idx++] = (3 << 30) | (3 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = COMPUTE_NUM_THREAD_X | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    ib[idx++] = x;
    ib[idx++] = y;
    ib[idx++] = z;
    
    // SET_SH_REG_INDEX for COMPUTE_START_X/Y/Z (Mini's requirement) with broadcast
    ib[idx++] = (3 << 30) | (3 << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = COMPUTE_START_X | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    ib[idx++] = 0;  // Start at 0
    ib[idx++] = 0;
    ib[idx++] = 0;
    
    return idx;
}

// Build DISPATCH_DIRECT packet with Mini's initiator settings
uint32_t sp_build_dispatch_direct(uint32_t* ib, uint32_t dim_x, uint32_t dim_y, uint32_t dim_z) {
    uint32_t idx = 0;
    
    // Add cache invalidation before dispatch (Mini's cache hygiene)
    ib[idx++] = (3 << 30) | (5 << 16) | PM4_ACQUIRE_MEM;
    ib[idx++] = 0x00000000;  // CP_COHER_CNTL
    ib[idx++] = 0xFFFFFFFF;  // CP_COHER_SIZE
    ib[idx++] = 0x00000000;  // CP_COHER_SIZE_HI
    ib[idx++] = 0x00000000;  // CP_COHER_BASE
    ib[idx++] = 0x00000000;  // CP_COHER_BASE_HI
    ib[idx++] = 0x0000000A;  // POLL_INTERVAL
    
    // DISPATCH_DIRECT with Mini's initiator bits
    ib[idx++] = (3 << 30) | (4 << 16) | PM4_DISPATCH_DIRECT;
    ib[idx++] = dim_x;
    ib[idx++] = dim_y;
    ib[idx++] = dim_z;
    
    // Initiator with Mini's required bits:
    // bit 0: COMPUTE_SHADER_EN = 1
    // bit 1: PARTIAL_TG_EN = 0
    // bit 2: FORCE_START_AT_000 = 1
    // bit 3: ORDERED_APPEND_ENBL = 0
    // bit 4: ORDERED_APPEND_MODE = 0
    // bit 5: USE_THREAD_DIMENSIONS = 0
    // bit 6: ORDER_MODE = 1
    // bit 7: DISPATCH_CACHE_CNTL = 0
    // bit 8: SCALAR_L1_INV_VOL = 0
    // bit 9: VECTOR_L1_INV_VOL = 0
    // bit 10: DATA_ATC = 0 (try without first)
    // bit 11: reserved
    // bit 12: RESTORE = 0
    uint32_t initiator = 0;
    initiator |= (1 << 0);  // COMPUTE_SHADER_EN
    initiator |= (1 << 2);  // FORCE_START_AT_000
    initiator |= (1 << 6);  // ORDER_MODE
    ib[idx++] = initiator;
    
    return idx;
}

// Set user data registers (for passing buffer addresses to shader)
uint32_t sp_set_user_data(uint32_t* ib, uint32_t start_reg, uint64_t* values, uint32_t count) {
    uint32_t idx = 0;
    
    // SET_SH_REG_INDEX for COMPUTE_USER_DATA_x with broadcast
    ib[idx++] = (3 << 30) | ((count * 2) << 16) | PM4_SET_SH_REG_INDEX;
    ib[idx++] = (COMPUTE_USER_DATA_0 + start_reg * 2) | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST;
    
    // Write pairs of low/high for each 64-bit value
    for (uint32_t i = 0; i < count; i++) {
        ib[idx++] = (uint32_t)(values[i] & 0xFFFFFFFF);
        ib[idx++] = (uint32_t)(values[i] >> 32);
    }
    
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
    
    // Add ACQUIRE_MEM to ensure shader completion and cache flush
    ib[idx++] = (3 << 30) | (5 << 16) | PM4_ACQUIRE_MEM;
    ib[idx++] = 0x00000000;  // CP_COHER_CNTL
    ib[idx++] = 0xFFFFFFFF;  // CP_COHER_SIZE
    ib[idx++] = 0x00000000;  // CP_COHER_SIZE_HI
    ib[idx++] = 0x00000000;  // CP_COHER_BASE
    ib[idx++] = 0x00000000;  // CP_COHER_BASE_HI
    ib[idx++] = 0x0000000A;  // POLL_INTERVAL
    
    return idx;
}