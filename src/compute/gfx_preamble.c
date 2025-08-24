// GFX Preamble for enabling compute CUs
// Must be run once per context on GFX ring before compute work

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "submit.h"

// PM4 packet types
#define PM4_SET_UCONFIG_REG     0x79
#define PM4_RELEASE_MEM         0x49

// UCONFIG registers
#define GRBM_GFX_INDEX          0x30800
#define COMPUTE_STATIC_THREAD_MGMT_SE0 0x30844
#define COMPUTE_STATIC_THREAD_MGMT_SE1 0x30848
#define COMPUTE_STATIC_THREAD_MGMT_SE2 0x3084C
#define COMPUTE_STATIC_THREAD_MGMT_SE3 0x30850

// GRBM_GFX_INDEX values
#define SE_BROADCAST_WRITES     (1 << 31)
#define SH_BROADCAST_WRITES     (1 << 29)
#define BROADCAST_ALL           (SE_BROADCAST_WRITES | SH_BROADCAST_WRITES)

// EOP event types
#define EOP_TC_ACTION_EN        (1 << 17)
#define EOP_TC_INV_EN           (1 << 20)
#define EOP_EVENT_WRITE_EOP     5

// Build GFX preamble IB
static uint32_t build_gfx_preamble(uint32_t* ib, uint64_t signal_va) {
    uint32_t idx = 0;
    
    // 1. Set GRBM_GFX_INDEX to broadcast mode
    ib[idx++] = (3 << 30) | (1 << 16) | PM4_SET_UCONFIG_REG;
    ib[idx++] = (GRBM_GFX_INDEX - 0x30000) >> 2;  // UCONFIG offset
    ib[idx++] = BROADCAST_ALL;
    
    // 2. Enable all CUs on all SEs
    ib[idx++] = (3 << 30) | (4 << 16) | PM4_SET_UCONFIG_REG;
    ib[idx++] = (COMPUTE_STATIC_THREAD_MGMT_SE0 - 0x30000) >> 2;
    ib[idx++] = 0xFFFFFFFF;  // SE0
    ib[idx++] = 0xFFFFFFFF;  // SE1
    ib[idx++] = 0xFFFFFFFF;  // SE2
    ib[idx++] = 0xFFFFFFFF;  // SE3
    
    // 3. RELEASE_MEM EOP to signal completion
    ib[idx++] = (3 << 30) | (5 << 16) | PM4_RELEASE_MEM;
    ib[idx++] = EOP_EVENT_WRITE_EOP | EOP_TC_ACTION_EN | EOP_TC_INV_EN;
    ib[idx++] = 0;  // dst_sel = memory
    ib[idx++] = (uint32_t)(signal_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)(signal_va >> 32);
    ib[idx++] = 1;  // data_lo = 1
    ib[idx++] = 0;  // data_hi = 0
    
    return idx;
}

// Submit GFX preamble to enable CUs
int sp_submit_gfx_preamble(sp_pm4_ctx* ctx) {
    printf("Submitting GFX preamble to enable CUs...\n");
    
    // Create signal BO
    sp_bo* signal_bo = sp_bo_new(ctx, 4096);
    if (!signal_bo) {
        printf("Failed to create signal BO\n");
        return -1;
    }
    
    // Clear signal
    uint32_t* signal_ptr = (uint32_t*)sp_bo_map(signal_bo);
    *signal_ptr = 0;
    sp_bo_unmap(signal_bo);
    
    // Create IB
    sp_bo* ib_bo = sp_bo_new(ctx, 4096);
    if (!ib_bo) {
        printf("Failed to create IB BO\n");
        sp_bo_free(signal_bo);
        return -1;
    }
    
    // Build preamble
    uint32_t* ib = (uint32_t*)sp_bo_map(ib_bo);
    uint32_t ib_size = build_gfx_preamble(ib, sp_bo_get_va(signal_bo));
    
    printf("GFX preamble IB (%d dwords):\n", ib_size);
    for (uint32_t i = 0; i < ib_size; i++) {
        printf("  [%02d] 0x%08X", i, ib[i]);
        if (i == 1) printf(" <- GRBM_GFX_INDEX offset");
        else if (i == 2) printf(" <- Broadcast mode");
        else if (i == 4) printf(" <- THREAD_MGMT_SE0 offset");
        else if (i == 5) printf(" <- Enable all CUs");
        else if (i == 13) printf(" <- Signal VA low");
        else if (i == 14) printf(" <- Signal VA high");
        printf("\n");
    }
    
    sp_bo_unmap(ib_bo);
    
    // Submit on GFX ring
    sp_bo* bos[] = {ib_bo, signal_bo};
    sp_fence fence;
    
    // HACK: For now, we'll use the compute submission path but need to
    // add proper GFX ring support. The kernel should have already enabled
    // CUs for us, so this is mainly for documentation.
    printf("\nNOTE: GFX ring submission not yet implemented.\n");
    printf("The kernel driver should have already enabled CUs.\n");
    printf("This code shows what would be needed for full control.\n");
    
    sp_bo_free(ib_bo);
    sp_bo_free(signal_bo);
    
    return 0;
}

// Check if CUs are enabled (for debugging)
void sp_check_cu_status(void) {
    printf("\nTo check CU status, run:\n");
    printf("  sudo umr -r *.*.GRBM_GFX_INDEX\n");
    printf("  sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE*\n");
    printf("\nExpected values:\n");
    printf("  GRBM_GFX_INDEX = 0xE0000000 (broadcast)\n");
    printf("  COMPUTE_STATIC_THREAD_MGMT_SE* = 0xFFFFFFFF\n");
}