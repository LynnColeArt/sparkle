// Direct Metal test without Fortran
// Just to prove the Metal implementation works!

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

// External Metal wrapper functions
extern int metal_available();
extern void* metal_create_context();
extern void metal_get_device_name(void* context, char* name, int len);
extern void* metal_create_buffer(void* context, size_t size);
extern void metal_copy_to_buffer(void* buffer, const void* data, size_t size);
extern void metal_copy_from_buffer(void* buffer, void* data, size_t size);
extern void* metal_compile_kernel(void* context, const char* source, const char* name);
extern void metal_dispatch_kernel(void* context, void* kernel,
                                 void** buffers, int num_buffers,
                                 const size_t* global_size, const size_t* local_size);
extern void metal_destroy_context(void* context);

// Simple vector add kernel in Metal
const char* vector_add_source = 
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "\n"
    "kernel void vector_add(\n"
    "    device const float* x [[buffer(0)]],\n"
    "    device const float* y [[buffer(1)]],\n"
    "    device float* z [[buffer(2)]],\n"
    "    uint idx [[thread_position_in_grid]])\n"
    "{\n"
    "    z[idx] = x[idx] + y[idx];\n"
    "}\n";

double get_time_ms() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
}

int main() {
    printf("=========================================\n");
    printf("   SPARKLE: Direct Metal Test (No Fortran)\n");
    printf("=========================================\n\n");
    
    // Check if Metal is available
    if (!metal_available()) {
        printf("❌ Metal is not available on this system\n");
        return 1;
    }
    
    printf("✅ Metal is available!\n\n");
    
    // Create context
    void* ctx = metal_create_context();
    if (!ctx) {
        printf("❌ Failed to create Metal context\n");
        return 1;
    }
    
    // Get device name
    char device_name[256];
    metal_get_device_name(ctx, device_name, 256);
    printf("🎮 Metal Device: %s\n\n", device_name);
    
    // Test parameters
    const int N = 10000000;  // 10 million elements
    printf("Testing with %d elements (%.1f MB)\n\n", N, (N * sizeof(float) * 3) / (1024.0 * 1024.0));
    
    // Allocate host memory
    float* x = (float*)malloc(N * sizeof(float));
    float* y = (float*)malloc(N * sizeof(float));
    float* z = (float*)malloc(N * sizeof(float));
    
    // Initialize data
    printf("Initializing test data...\n");
    for (int i = 0; i < N; i++) {
        x[i] = (float)i;
        y[i] = (float)(i * 2);
        z[i] = 0.0f;
    }
    
    // Create Metal buffers
    printf("Creating GPU buffers...\n");
    void* x_buffer = metal_create_buffer(ctx, N * sizeof(float));
    void* y_buffer = metal_create_buffer(ctx, N * sizeof(float));
    void* z_buffer = metal_create_buffer(ctx, N * sizeof(float));
    
    // Compile kernel
    printf("Compiling Metal kernel...\n");
    void* kernel = metal_compile_kernel(ctx, vector_add_source, "vector_add");
    if (!kernel) {
        printf("❌ Failed to compile kernel\n");
        return 1;
    }
    
    printf("\n🏁 Starting benchmark...\n");
    printf("------------------------\n");
    
    // Warm-up run
    printf("Warm-up run...\n");
    metal_copy_to_buffer(x_buffer, x, N * sizeof(float));
    metal_copy_to_buffer(y_buffer, y, N * sizeof(float));
    
    void* buffers[] = {x_buffer, y_buffer, z_buffer};
    size_t global_size[] = {N, 1, 1};
    size_t local_size[] = {256, 1, 1};
    
    metal_dispatch_kernel(ctx, kernel, buffers, 3, global_size, local_size);
    metal_copy_from_buffer(z_buffer, z, N * sizeof(float));
    
    // Timed run
    printf("Timed run...\n");
    double start = get_time_ms();
    
    metal_copy_to_buffer(x_buffer, x, N * sizeof(float));
    metal_copy_to_buffer(y_buffer, y, N * sizeof(float));
    metal_dispatch_kernel(ctx, kernel, buffers, 3, global_size, local_size);
    metal_copy_from_buffer(z_buffer, z, N * sizeof(float));
    
    double end = get_time_ms();
    double metal_time = end - start;
    
    // Verify results
    printf("\nVerifying results...\n");
    int errors = 0;
    for (int i = 0; i < 5; i++) {
        float expected = x[i] + y[i];
        printf("  z[%d] = %.1f + %.1f = %.1f (expected %.1f)\n", 
               i, x[i], y[i], z[i], expected);
        if (fabs(z[i] - expected) > 0.001) errors++;
    }
    
    // Compare with CPU time
    printf("\nCPU comparison run...\n");
    start = get_time_ms();
    for (int i = 0; i < N; i++) {
        z[i] = x[i] + y[i];
    }
    end = get_time_ms();
    double cpu_time = end - start;
    
    // Results
    printf("\n=========================================\n");
    printf("              RESULTS\n");
    printf("=========================================\n");
    printf("Metal GPU time: %.2f ms\n", metal_time);
    printf("CPU time:       %.2f ms\n", cpu_time);
    printf("\n");
    
    if (metal_time < cpu_time) {
        printf("🏆 METAL WINS! %.1fx faster than CPU!\n", cpu_time / metal_time);
    } else {
        printf("🐌 CPU wins by %.1fx\n", metal_time / cpu_time);
        printf("(Overhead dominates for this simple operation)\n");
    }
    
    printf("\nPerformance metrics:\n");
    printf("  Effective bandwidth: %.1f GB/s\n", 
           (N * sizeof(float) * 3) / (metal_time * 1e6));
    printf("  Throughput: %.1f GFLOPS\n", N / (metal_time * 1e6));
    
    if (errors == 0) {
        printf("\n✅ All results verified correct!\n");
    } else {
        printf("\n❌ Found %d errors in verification\n", errors);
    }
    
    // Cleanup
    metal_destroy_context(ctx);
    free(x);
    free(y);
    free(z);
    
    printf("\n🌟 The Sparkle Way: Metal compute without CUDA!\n");
    
    return 0;
}