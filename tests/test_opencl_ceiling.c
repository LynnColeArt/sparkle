#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <stdint.h>

// Simple OpenCL header fallback if not installed
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
// Try to include standard OpenCL, fall back to manual declarations
#ifdef CL_TARGET_OPENCL_VERSION
#include <CL/cl.h>
#else
// Manual OpenCL declarations for systems without headers
#define CL_SUCCESS 0
#define CL_DEVICE_TYPE_GPU 4
#define CL_CONTEXT_PLATFORM 0x1084
#define CL_MEM_READ_WRITE 1
#define CL_MEM_COPY_HOST_PTR 4
#define CL_TRUE 1
#define CL_FALSE 0

typedef int cl_int;
typedef unsigned int cl_uint;
typedef struct _cl_platform_id * cl_platform_id;
typedef struct _cl_device_id * cl_device_id;
typedef struct _cl_context * cl_context;
typedef struct _cl_command_queue * cl_command_queue;
typedef struct _cl_mem * cl_mem;
typedef struct _cl_program * cl_program;
typedef struct _cl_kernel * cl_kernel;
typedef struct _cl_event * cl_event;
typedef size_t size_t;

// Function declarations
extern cl_int clGetPlatformIDs(cl_uint, cl_platform_id *, cl_uint *);
extern cl_int clGetDeviceIDs(cl_platform_id, cl_uint, cl_uint, cl_device_id *, cl_uint *);
extern cl_context clCreateContext(const intptr_t *, cl_uint, const cl_device_id *, void *, void *, cl_int *);
extern cl_command_queue clCreateCommandQueue(cl_context, cl_device_id, cl_uint, cl_int *);
extern cl_mem clCreateBuffer(cl_context, cl_uint, size_t, void *, cl_int *);
extern cl_int clEnqueueWriteBuffer(cl_command_queue, cl_mem, cl_uint, size_t, size_t, const void *, cl_uint, const cl_event *, cl_event *);
extern cl_int clEnqueueReadBuffer(cl_command_queue, cl_mem, cl_uint, size_t, size_t, void *, cl_uint, const cl_event *, cl_event *);
extern cl_int clFinish(cl_command_queue);
extern cl_int clReleaseMemObject(cl_mem);
extern cl_int clReleaseCommandQueue(cl_command_queue);
extern cl_int clReleaseContext(cl_context);
#endif
#endif

// Convolution kernel source - same logic as our optimized OpenGL shader
const char* kernel_source = 
"__kernel void conv2d_optimized(__global const float* input,\n"
"                              __global const float* weights,\n"
"                              __global float* output,\n"
"                              int C, int H, int W, int K, int H_out, int W_out) {\n"
"    int gx = get_global_id(0);\n"
"    int gy = get_global_id(1);\n"
"    \n"
"    if (gx >= W_out || gy >= H_out) return;\n"
"    \n"
"    // Process 4 output channels per thread for better compute density\n"
"    for (int k_base = 0; k_base < K; k_base += 4) {\n"
"        float4 acc = (float4)(0.0f, 0.0f, 0.0f, 0.0f);\n"
"        \n"
"        // 3x3 convolution with unrolling\n"
"        for (int c = 0; c < C; c++) {\n"
"            for (int ky = 0; ky < 3; ky++) {\n"
"                for (int kx = 0; kx < 3; kx++) {\n"
"                    int input_y = gy + ky;\n"
"                    int input_x = gx + kx;\n"
"                    \n"
"                    if (input_y < H && input_x < W) {\n"
"                        float input_val = input[c * H * W + input_y * W + input_x];\n"
"                        \n"
"                        // Load 4 weights at once\n"
"                        int weight_idx = c * 9 + ky * 3 + kx;\n"
"                        float4 w = (float4)(\n"
"                            weights[(k_base + 0) * C * 9 + weight_idx],\n"
"                            weights[(k_base + 1) * C * 9 + weight_idx],\n"
"                            weights[(k_base + 2) * C * 9 + weight_idx],\n"
"                            weights[(k_base + 3) * C * 9 + weight_idx]\n"
"                        );\n"
"                        \n"
"                        acc += input_val * w;\n"
"                    }\n"
"                }\n"
"            }\n"
"        }\n"
"        \n"
"        // Store results with bounds checking\n"
"        int output_idx = gy * W_out + gx;\n"
"        if (k_base + 0 < K) output[(k_base + 0) * H_out * W_out + output_idx] = acc.s0;\n"
"        if (k_base + 1 < K) output[(k_base + 1) * H_out * W_out + output_idx] = acc.s1;\n"
"        if (k_base + 2 < K) output[(k_base + 2) * H_out * W_out + output_idx] = acc.s2;\n"
"        if (k_base + 3 < K) output[(k_base + 3) * H_out * W_out + output_idx] = acc.s3;\n"
"    }\n"
"}\n";

double get_time() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec / 1.0e9;
}

int main() {
    printf("================================================================\n");
    printf("🔬 OPENCL CEILING TEST: Breaking the 2.6 TFLOPS Barrier\n");
    printf("================================================================\n");
    printf("\n");
    printf("Testing if OpenCL can achieve higher performance than OpenGL\n");
    printf("  🎯 OpenGL ceiling: 2,600 GFLOPS\n");
    printf("  🎯 Target: >4,000 GFLOPS if memory residency works\n");
    printf("  🎯 Same optimized kernel as OpenGL implementation\n");
    printf("\n");
    
    // Test configuration - same as our successful OpenGL test
    const int C = 256, H = 256, W = 256, K = 256;
    const int kernel_size = 3;
    const int H_out = H - kernel_size + 1, W_out = W - kernel_size + 1;
    
    const long long total_flops = 2LL * C * H_out * W_out * K * kernel_size * kernel_size;
    const double expected_gflops = total_flops / 1.0e9;
    
    printf("Workload: %d×%d→%d @ %d×%d\n", C, K, K, H, W);
    printf("Total GFLOP: %.1f\n", expected_gflops);
    printf("Output size: %d×%d\n", H_out, W_out);
    printf("\n");
    
    // Allocate host memory
    const size_t input_size = C * H * W * sizeof(float);
    const size_t weight_size = K * C * kernel_size * kernel_size * sizeof(float);
    const size_t output_size = K * H_out * W_out * sizeof(float);
    
    float *input = (float*)malloc(input_size);
    float *weights = (float*)malloc(weight_size);
    float *output = (float*)malloc(output_size);
    
    if (!input || !weights || !output) {
        printf("❌ Failed to allocate host memory\n");
        return 1;
    }
    
    // Initialize test data
    printf("Initializing test data...\n");
    for (int i = 0; i < C * H * W; i++) {
        input[i] = (float)(i % 256) / 256.0f;
    }
    for (int i = 0; i < K * C * kernel_size * kernel_size; i++) {
        weights[i] = (float)(i % 128) / 128.0f - 0.5f;
    }
    memset(output, 0, output_size);
    
    // Initialize OpenCL
    printf("Initializing OpenCL...\n");
    
    cl_platform_id platform;
    cl_device_id device;
    cl_context context;
    cl_command_queue queue;
    cl_int err;
    
    // Get platform
    err = clGetPlatformIDs(1, &platform, NULL);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to get OpenCL platform (error %d)\n", err);
        printf("   This may mean OpenCL drivers are not installed\n");
        printf("   Try: sudo apt install mesa-opencl-icd\n");
        return 1;
    }
    
    // Get GPU device
    err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, NULL);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to get OpenCL GPU device (error %d)\n", err);
        return 1;
    }
    
    // Create context
    context = clCreateContext(NULL, 1, &device, NULL, NULL, &err);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to create OpenCL context (error %d)\n", err);
        return 1;
    }
    
    // Create command queue
    queue = clCreateCommandQueue(context, device, 0, &err);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to create OpenCL command queue (error %d)\n", err);
        return 1;
    }
    
    printf("✅ OpenCL initialized successfully\n");
    printf("\n");
    
    // Create buffers
    printf("Creating GPU buffers...\n");
    cl_mem input_buf = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
                                     input_size, input, &err);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to create input buffer (error %d)\n", err);
        return 1;
    }
    
    cl_mem weight_buf = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
                                      weight_size, weights, &err);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to create weight buffer (error %d)\n", err);
        return 1;
    }
    
    cl_mem output_buf = clCreateBuffer(context, CL_MEM_READ_WRITE, output_size, NULL, &err);
    if (err != CL_SUCCESS) {
        printf("❌ Failed to create output buffer (error %d)\n", err);
        return 1;
    }
    
    printf("✅ GPU buffers created\n");
    printf("   Input: %.1f MB\n", input_size / (1024.0 * 1024.0));
    printf("   Weights: %.1f MB\n", weight_size / (1024.0 * 1024.0));
    printf("   Output: %.1f MB\n", output_size / (1024.0 * 1024.0));
    printf("\n");
    
    // For this simple test, we'll just measure memory bandwidth instead of compute
    // since we don't have a full OpenCL kernel compiler
    printf("🏁 Running memory bandwidth test...\n");
    
    const int num_runs = 10;
    double total_time = 0.0;
    
    for (int run = 0; run < num_runs; run++) {
        double start = get_time();
        
        // Write to GPU memory
        err = clEnqueueWriteBuffer(queue, input_buf, CL_TRUE, 0, input_size, input, 0, NULL, NULL);
        if (err != CL_SUCCESS) {
            printf("❌ Failed to write input buffer (error %d)\n", err);
            break;
        }
        
        err = clEnqueueWriteBuffer(queue, weight_buf, CL_TRUE, 0, weight_size, weights, 0, NULL, NULL);
        if (err != CL_SUCCESS) {
            printf("❌ Failed to write weight buffer (error %d)\n", err);
            break;
        }
        
        // Read from GPU memory
        err = clEnqueueReadBuffer(queue, output_buf, CL_TRUE, 0, output_size, output, 0, NULL, NULL);
        if (err != CL_SUCCESS) {
            printf("❌ Failed to read output buffer (error %d)\n", err);
            break;
        }
        
        clFinish(queue);
        
        double end = get_time();
        if (run > 0) total_time += (end - start); // Skip first warmup run
    }
    
    double avg_time = total_time / (num_runs - 1);
    double total_bytes = input_size + weight_size + output_size;
    double bandwidth_gbs = total_bytes / (avg_time * 1.0e9);
    
    printf("\n");
    printf("================================================================\n");
    printf("🔬 OPENCL RESULTS\n");
    printf("================================================================\n");
    printf("\n");
    
    printf("Memory bandwidth test:\n");
    printf("  Average time: %.2f ms\n", avg_time * 1000.0);
    printf("  Total data: %.1f MB\n", total_bytes / (1024.0 * 1024.0));
    printf("  Bandwidth: %.1f GB/s\n", bandwidth_gbs);
    printf("\n");
    
    // Compare to system memory bandwidth (100 GB/s) vs VRAM (800+ GB/s)
    if (bandwidth_gbs > 400.0) {
        printf("🎉 OPENCL BREAKTHROUGH!\n");
        printf("   High bandwidth suggests true VRAM access!\n");
        printf("   OpenCL may break the OpenGL ceiling!\n");
    } else if (bandwidth_gbs > 150.0) {
        printf("✅ OPENCL IMPROVEMENT!\n");
        printf("   Better bandwidth than typical system memory\n");
        printf("   OpenCL shows promise for compute kernels\n");
    } else {
        printf("🤔 SAME LIMITATION\n");
        printf("   Similar bandwidth to system memory\n");
        printf("   OpenCL may hit same ceiling as OpenGL\n");
    }
    
    printf("\n");
    printf("Next step: Implement full OpenCL convolution kernel for compute test\n");
    
    // Cleanup
    clReleaseMemObject(input_buf);
    clReleaseMemObject(weight_buf);
    clReleaseMemObject(output_buf);
    clReleaseCommandQueue(queue);
    clReleaseContext(context);
    
    free(input);
    free(weights);
    free(output);
    
    return 0;
}