// Simple GPU convolution test using the reference implementation
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// From gpu_opengl_reference.c
extern float gpu_execute_conv2d_fortran(
    const float* input, const float* weights, float* output,
    int N, int C, int H, int W, int K, 
    int kernel_size, int stride, int pad,
    int H_out, int W_out
);

float randf() {
    return (float)rand() / RAND_MAX * 2.0f - 1.0f;
}

int main() {
    printf("🔦 Layer 3: Direct GPU Performance Test\n");
    printf("=====================================\n\n");
    
    // Test parameters
    int N = 1, C = 64, H = 56, W = 56;
    int K = 64, kernel_size = 3, stride = 1, pad = 1;
    int H_out = 56, W_out = 56;
    
    // Calculate sizes
    int input_size = N * C * H * W;
    int weight_size = K * C * kernel_size * kernel_size;
    int output_size = N * K * H_out * W_out;
    
    printf("📊 Test Configuration:\n");
    printf(" Input: %dx%dx%dx%d\n", N, C, H, W);
    printf(" Kernel: %dx%d\n", kernel_size, kernel_size);
    printf(" Output: %dx%dx%dx%d\n", N, K, H_out, W_out);
    printf("\n");
    
    // Allocate arrays
    float* input = (float*)malloc(input_size * sizeof(float));
    float* weights = (float*)malloc(weight_size * sizeof(float));
    float* output = (float*)malloc(output_size * sizeof(float));
    
    // Initialize with random data
    srand(time(NULL));
    for (int i = 0; i < input_size; i++) {
        input[i] = randf();
    }
    for (int i = 0; i < weight_size; i++) {
        weights[i] = randf() * 0.1f;
    }
    
    printf("🎮 Running GPU convolution...\n");
    
    // Warm up
    float exec_time_ms = gpu_execute_conv2d_fortran(input, weights, output, 
                                                    N, C, H, W, K, kernel_size, stride, pad, H_out, W_out);
    
    if (exec_time_ms < 0) {
        printf("❌ GPU execution failed\n");
        free(input);
        free(weights);
        free(output);
        return 1;
    }
    
    // Actual benchmark
    exec_time_ms = gpu_execute_conv2d_fortran(input, weights, output, 
                                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out);
    
    if (exec_time_ms > 0) {
        // Calculate GFLOPS
        long long total_flops = (long long)N * K * H_out * W_out * C * kernel_size * kernel_size * 2;
        float gflops = (float)total_flops / (exec_time_ms * 1e6);
        
        printf("\n✅ GPU Performance Results:\n");
        printf("   Execution time: %.2f ms\n", exec_time_ms);
        printf("   Performance: %.1f GFLOPS\n", gflops);
        printf("\n");
        printf("   Expected on RX 7900 XTX: ~451 GFLOPS\n");
    } else {
        printf("❌ GPU execution failed\n");
    }
    
    // Cleanup
    free(input);
    free(weights);
    free(output);
    
    return 0;
}