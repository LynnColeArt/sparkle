// Vulkan VRAM Bandwidth Test
// ==========================
// Test raw memory bandwidth to verify VRAM performance

#include <vulkan/vulkan.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// External globals
extern VkInstance g_instance;
extern VkPhysicalDevice g_physical_device;
extern VkDevice g_device;
extern VkQueue g_compute_queue;
extern VkCommandPool g_command_pool;
extern uint32_t g_queue_family_index;

// Simple bandwidth test shader (just copies data)
const char* bandwidth_shader = 
    "#version 450\n"
    "layout(local_size_x = 256) in;\n"
    "layout(binding = 0) readonly buffer Input { float data[]; } input_buf;\n"
    "layout(binding = 1) writeonly buffer Output { float data[]; } output_buf;\n"
    "void main() {\n"
    "    uint idx = gl_GlobalInvocationID.x;\n"
    "    output_buf.data[idx] = input_buf.data[idx];\n"
    "}\n";

int main() {
    printf("🚀 Vulkan VRAM Bandwidth Test\n");
    printf("=============================\n\n");
    
    // Initialize Vulkan (reuse existing backend)
    extern int vk_init();
    if (!vk_init()) {
        printf("❌ Failed to initialize Vulkan\n");
        return 1;
    }
    
    // Test parameters
    const size_t BUFFER_SIZE = 1024 * 1024 * 1024; // 1 GB
    const int ITERATIONS = 100;
    
    printf("📊 Test configuration:\n");
    printf("  Buffer size: %.1f GB\n", BUFFER_SIZE / (1024.0 * 1024.0 * 1024.0));
    printf("  Iterations: %d\n", ITERATIONS);
    printf("  Total data: %.1f GB\n\n", (BUFFER_SIZE * ITERATIONS) / (1024.0 * 1024.0 * 1024.0));
    
    // Allocate buffers using our backend
    extern void* vk_allocate_buffer_with_staging(size_t size, int device_local);
    
    printf("🎯 Allocating buffers...\n");
    void* input_buf = vk_allocate_buffer_with_staging(BUFFER_SIZE, 1);
    void* output_buf = vk_allocate_buffer_with_staging(BUFFER_SIZE, 1);
    
    if (!input_buf || !output_buf) {
        printf("❌ Failed to allocate buffers\n");
        return 1;
    }
    
    // Initialize input buffer with data
    float* test_data = malloc(BUFFER_SIZE);
    for (size_t i = 0; i < BUFFER_SIZE/sizeof(float); i++) {
        test_data[i] = (float)i;
    }
    
    extern void vk_upload_buffer_data(void* buffer, const void* data, size_t size);
    vk_upload_buffer_data(input_buf, test_data, BUFFER_SIZE);
    free(test_data);
    
    printf("✅ Buffers allocated and initialized\n\n");
    
    // Create simple copy shader
    extern void* vk_compile_shader(const void* spirv_data, size_t spirv_size);
    
    // Compile shader to SPIR-V
    system("echo '" bandwidth_shader "' > /tmp/bandwidth.comp && "
           "glslc -fshader-stage=compute /tmp/bandwidth.comp -o /tmp/bandwidth.spv");
    
    FILE* f = fopen("/tmp/bandwidth.spv", "rb");
    fseek(f, 0, SEEK_END);
    size_t spirv_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    void* spirv_data = malloc(spirv_size);
    fread(spirv_data, 1, spirv_size, f);
    fclose(f);
    
    void* shader = vk_compile_shader(spirv_data, spirv_size);
    free(spirv_data);
    
    if (!shader) {
        printf("❌ Failed to compile shader\n");
        return 1;
    }
    
    printf("✅ Bandwidth test shader compiled\n\n");
    
    // Run bandwidth test
    printf("🏃 Running bandwidth test...\n");
    
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    // Dispatch many times
    for (int i = 0; i < ITERATIONS; i++) {
        // Simple dispatch without timing overhead
        extern float vk_dispatch_compute(void* shader, void* input, void* weights, void* output,
                                       int x, int y, int z);
        
        int num_elements = BUFFER_SIZE / sizeof(float);
        int workgroups = (num_elements + 255) / 256;
        
        vk_dispatch_compute(shader, input_buf, NULL, output_buf, workgroups, 1, 1);
        
        if (i % 10 == 0) {
            printf("  Iteration %d/%d\r", i+1, ITERATIONS);
            fflush(stdout);
        }
    }
    
    // Wait for all operations to complete
    vkQueueWaitIdle(g_compute_queue);
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) * 1e-9;
    
    printf("\n\n📊 Results:\n");
    printf("  Total time: %.2f seconds\n", elapsed);
    printf("  Data transferred: %.1f GB\n", (BUFFER_SIZE * ITERATIONS) / (1024.0 * 1024.0 * 1024.0));
    printf("  Bandwidth: %.1f GB/s\n", (BUFFER_SIZE * ITERATIONS) / (1024.0 * 1024.0 * 1024.0) / elapsed);
    
    double expected_vram = 960.0; // GB/s theoretical
    double measured = (BUFFER_SIZE * ITERATIONS) / (1024.0 * 1024.0 * 1024.0) / elapsed;
    printf("\n🎯 Analysis:\n");
    printf("  Theoretical VRAM bandwidth: %.0f GB/s\n", expected_vram);
    printf("  Measured bandwidth: %.1f GB/s (%.1f%% efficiency)\n", 
           measured, (measured / expected_vram) * 100);
    
    if (measured > 100) {
        printf("  ✅ This is definitely VRAM bandwidth!\n");
    } else if (measured > 20) {
        printf("  ⚡ This might be PCIe bandwidth\n");
    } else {
        printf("  🐌 This looks like system RAM bandwidth\n");
    }
    
    // Cleanup
    extern void vk_free_buffer_full(void* buffer);
    extern void vk_free_shader(void* shader);
    extern void vk_cleanup();
    
    vk_free_buffer_full(input_buf);
    vk_free_buffer_full(output_buf);
    vk_free_shader(shader);
    vk_cleanup();
    
    return 0;
}