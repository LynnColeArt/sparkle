// Test Metal with more complex computation
// Where GPU should actually win!

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

extern int metal_available();
extern void* metal_create_context();
extern void* metal_create_buffer(void* context, size_t size);
extern void metal_copy_to_buffer(void* buffer, const void* data, size_t size);
extern void metal_copy_from_buffer(void* buffer, void* data, size_t size);
extern void* metal_compile_kernel(void* context, const char* source, const char* name);
extern void metal_dispatch_kernel(void* context, void* kernel,
                                 void** buffers, int num_buffers,
                                 const size_t* global_size, const size_t* local_size);
extern void metal_destroy_context(void* context);

// Complex kernel: More arithmetic intensity
const char* complex_kernel = 
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "\n"
    "kernel void complex_compute(\n"
    "    device const float* x [[buffer(0)]],\n"
    "    device const float* y [[buffer(1)]],\n"
    "    device float* z [[buffer(2)]],\n"
    "    uint idx [[thread_position_in_grid]])\n"
    "{\n"
    "    float a = x[idx];\n"
    "    float b = y[idx];\n"
    "    float result = 0.0;\n"
    "    \n"
    "    // Do 100 operations per element (increase arithmetic intensity)\n"
    "    for (int i = 0; i < 100; i++) {\n"
    "        result += sqrt(a * a + b * b);\n"
    "        a = sin(a) * cos(b);\n"
    "        b = cos(result) * sin(a);\n"
    "    }\n"
    "    z[idx] = result;\n"
    "}\n";

double get_time_ms() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
}

int main() {
    printf("=========================================\n");
    printf("   SPARKLE: Complex Computation Test\n");
    printf("=========================================\n\n");
    
    void* ctx = metal_create_context();
    const int N = 1000000;  // 1 million elements
    
    printf("Testing with %d elements\n", N);
    printf("100 trig operations per element\n\n");
    
    // Allocate and initialize
    float* x = (float*)malloc(N * sizeof(float));
    float* y = (float*)malloc(N * sizeof(float));
    float* z_gpu = (float*)calloc(N, sizeof(float));
    float* z_cpu = (float*)calloc(N, sizeof(float));
    
    for (int i = 0; i < N; i++) {
        x[i] = (float)(i % 100) / 100.0f;
        y[i] = (float)(i % 50) / 50.0f;
    }
    
    // GPU buffers
    void* x_buffer = metal_create_buffer(ctx, N * sizeof(float));
    void* y_buffer = metal_create_buffer(ctx, N * sizeof(float));
    void* z_buffer = metal_create_buffer(ctx, N * sizeof(float));
    
    // Compile kernel
    void* kernel = metal_compile_kernel(ctx, complex_kernel, "complex_compute");
    
    // GPU execution
    printf("Running on GPU...\n");
    double start = get_time_ms();
    
    metal_copy_to_buffer(x_buffer, x, N * sizeof(float));
    metal_copy_to_buffer(y_buffer, y, N * sizeof(float));
    
    void* buffers[] = {x_buffer, y_buffer, z_buffer};
    size_t global_size[] = {N, 1, 1};
    size_t local_size[] = {256, 1, 1};
    metal_dispatch_kernel(ctx, kernel, buffers, 3, global_size, local_size);
    
    metal_copy_from_buffer(z_buffer, z_gpu, N * sizeof(float));
    
    double gpu_time = get_time_ms() - start;
    
    // CPU execution
    printf("Running on CPU...\n");
    start = get_time_ms();
    
    for (int j = 0; j < N; j++) {
        float a = x[j];
        float b = y[j];
        float result = 0.0;
        
        for (int i = 0; i < 100; i++) {
            result += sqrt(a * a + b * b);
            a = sin(a) * cos(b);
            b = cos(result) * sin(a);
        }
        z_cpu[j] = result;
    }
    
    double cpu_time = get_time_ms() - start;
    
    // Results
    printf("\n=========================================\n");
    printf("              RESULTS\n");
    printf("=========================================\n");
    printf("Metal GPU time: %.2f ms\n", gpu_time);
    printf("CPU time:       %.2f ms\n", cpu_time);
    printf("\n");
    
    if (gpu_time < cpu_time) {
        printf("🏆 GPU WINS! %.1fx faster than CPU!\n", cpu_time / gpu_time);
        printf("\nThis is where GPUs shine - compute-intensive work!\n");
    } else {
        printf("CPU still wins by %.1fx\n", gpu_time / cpu_time);
    }
    
    printf("\nOperations: %d million trig ops\n", (N * 100 * 4) / 1000000);
    printf("GPU GFLOPS: %.1f\n", (N * 100 * 4) / (gpu_time * 1e6));
    printf("CPU GFLOPS: %.1f\n", (N * 100 * 4) / (cpu_time * 1e6));
    
    // Cleanup
    metal_destroy_context(ctx);
    free(x); free(y); free(z_gpu); free(z_cpu);
    
    return 0;
}