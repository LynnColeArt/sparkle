// Sporkle CLI main entry point

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../compute/submit.h"

// External selftest function
extern int sp_pm4_selftest(sp_pm4_ctx* ctx);

void print_usage(const char* prog) {
    printf("Usage: %s <command> [options]\n", prog);
    printf("\n");
    printf("Commands:\n");
    printf("  pm4 --selftest    Run PM4 compute self-test\n");
    printf("  pm4 --info        Show PM4 device information\n");
    printf("  help              Show this help message\n");
    printf("\n");
}

int cmd_pm4_selftest() {
    // Create PM4 context
    sp_pm4_ctx* ctx = sp_pm4_ctx_create();
    if (!ctx) {
        fprintf(stderr, "Failed to create PM4 context\n");
        return 1;
    }
    
    // Run selftest
    int ret = sp_pm4_selftest(ctx);
    
    // Cleanup
    sp_pm4_ctx_destroy(ctx);
    
    return ret;
}

int cmd_pm4_info() {
    // Create PM4 context
    sp_pm4_ctx* ctx = sp_pm4_ctx_create();
    if (!ctx) {
        fprintf(stderr, "Failed to create PM4 context\n");
        return 1;
    }
    
    // Print device info
    printf("PM4 Device Information:\n");
    printf("  Device ID: 0x%04x\n", ctx->device_id);
    printf("  Compute rings: %d\n", ctx->num_compute_rings);
    
    // TODO: Add more detailed info (ASIC name, architecture, etc.)
    
    sp_pm4_ctx_destroy(ctx);
    return 0;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }
    
    // Parse command
    if (strcmp(argv[1], "pm4") == 0 && argc >= 3) {
        if (strcmp(argv[2], "--selftest") == 0) {
            return cmd_pm4_selftest();
        } else if (strcmp(argv[2], "--info") == 0) {
            return cmd_pm4_info();
        }
    } else if (strcmp(argv[1], "help") == 0) {
        print_usage(argv[0]);
        return 0;
    }
    
    fprintf(stderr, "Unknown command: %s\n", argv[1]);
    print_usage(argv[0]);
    return 1;
}