#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "src/compute/submit.h"

int main() {
    printf("=== PM4 Selftest Runner ===\n\n");
    
    // Create PM4 context
    sp_pm4_ctx* ctx = sp_pm4_ctx_create();
    if (!ctx) {
        printf("❌ Failed to create PM4 context\n");
        return 1;
    }
    
    // Get device info
    sp_device_info info;
    if (sp_pm4_get_device_info(ctx, &info) == 0) {
        printf("Device: %s (0x%04x)\n", info.name, info.device_id);
        printf("Compute rings: %d\n", info.num_compute_rings);
        printf("VRAM: %lu MB\n", info.vram_size / (1024*1024));
        printf("\n");
    }
    
    // Check if we should scan rings
    const char* scan_rings = getenv("PM4_SCAN_RINGS");
    if (scan_rings && strcmp(scan_rings, "1") == 0) {
        printf("Scanning all compute rings...\n");
        for (int ring = 0; ring < info.num_compute_rings; ring++) {
            printf("\n=== Testing Ring %d ===\n", ring);
            char ring_str[16];
            snprintf(ring_str, sizeof(ring_str), "%d", ring);
            setenv("PM4_COMPUTE_RING", ring_str, 1);
            
            // Create new context for this ring
            sp_pm4_ctx* ring_ctx = sp_pm4_ctx_create();
            if (ring_ctx) {
                int ring_result = sp_pm4_selftest(ring_ctx);
                if (ring_result == 0) {
                    printf("✅ Ring %d WORKS!\n", ring);
                    sp_pm4_ctx_destroy(ring_ctx);
                    sp_pm4_ctx_destroy(ctx);
                    return 0;
                }
                sp_pm4_ctx_destroy(ring_ctx);
            }
        }
        printf("\n❌ No working rings found\n");
    } else {
        // Run normal selftest
        printf("Running selftest on default ring (set PM4_SCAN_RINGS=1 to scan all)\n");
        int result = sp_pm4_selftest(ctx);
        sp_pm4_ctx_destroy(ctx);
        return result;
    }
    
    // Cleanup
    sp_pm4_ctx_destroy(ctx);
    
    return 1;
}