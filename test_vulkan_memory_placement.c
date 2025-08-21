// Test Vulkan Memory Placement
// ============================
// Quick test to verify if buffers are truly in VRAM

#include <vulkan/vulkan.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// Test parameters
#define BUFFER_SIZE (256 * 1024 * 1024)  // 256 MB
#define ITERATIONS 100

// Simple timing
double get_time() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}

int main() {
    printf("🔍 Vulkan Memory Placement Test\n");
    printf("===============================\n\n");
    
    // Initialize Vulkan
    VkInstance instance;
    VkInstanceCreateInfo instance_info = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &(VkApplicationInfo){
            .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
            .pApplicationName = "Memory Test",
            .apiVersion = VK_API_VERSION_1_2
        }
    };
    
    if (vkCreateInstance(&instance_info, NULL, &instance) != VK_SUCCESS) {
        printf("❌ Failed to create instance\n");
        return 1;
    }
    
    // Get physical device
    uint32_t device_count = 1;
    VkPhysicalDevice physical_device;
    vkEnumeratePhysicalDevices(instance, &device_count, &physical_device);
    
    // Get device properties
    VkPhysicalDeviceProperties props;
    vkGetPhysicalDeviceProperties(physical_device, &props);
    printf("📊 GPU: %s\n", props.deviceName);
    
    // Get memory properties
    VkPhysicalDeviceMemoryProperties mem_props;
    vkGetPhysicalDeviceMemoryProperties(physical_device, &mem_props);
    
    printf("\n📍 Memory Heaps:\n");
    for (uint32_t i = 0; i < mem_props.memoryHeapCount; i++) {
        printf("  Heap %d: %.1f GB %s\n", i, 
               mem_props.memoryHeaps[i].size / (1024.0 * 1024.0 * 1024.0),
               (mem_props.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) ? "DEVICE_LOCAL" : "SYSTEM");
    }
    
    printf("\n📍 Memory Types:\n");
    for (uint32_t i = 0; i < mem_props.memoryTypeCount; i++) {
        printf("  Type %d (Heap %d): ", i, mem_props.memoryTypes[i].heapIndex);
        VkMemoryPropertyFlags flags = mem_props.memoryTypes[i].propertyFlags;
        if (flags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) printf("DEVICE_LOCAL ");
        if (flags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) printf("HOST_VISIBLE ");
        if (flags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) printf("HOST_COHERENT ");
        if (flags & VK_MEMORY_PROPERTY_HOST_CACHED_BIT) printf("HOST_CACHED ");
        printf("\n");
    }
    
    // Create device
    float queue_priority = 1.0f;
    VkDevice device;
    VkDeviceCreateInfo device_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .queueCreateInfoCount = 1,
        .pQueueCreateInfos = &(VkDeviceQueueCreateInfo){
            .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
            .queueFamilyIndex = 0,
            .queueCount = 1,
            .pQueuePriorities = &queue_priority
        }
    };
    
    if (vkCreateDevice(physical_device, &device_info, NULL, &device) != VK_SUCCESS) {
        printf("❌ Failed to create device\n");
        vkDestroyInstance(instance, NULL);
        return 1;
    }
    
    // Test memory allocation
    printf("\n🧪 Testing buffer allocation (256 MB)...\n");
    
    // Create buffer
    VkBuffer buffer;
    VkBufferCreateInfo buffer_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = BUFFER_SIZE,
        .usage = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT
    };
    
    if (vkCreateBuffer(device, &buffer_info, NULL, &buffer) != VK_SUCCESS) {
        printf("❌ Failed to create buffer\n");
        vkDestroyDevice(device, NULL);
        vkDestroyInstance(instance, NULL);
        return 1;
    }
    
    // Get memory requirements
    VkMemoryRequirements mem_reqs;
    vkGetBufferMemoryRequirements(device, buffer, &mem_reqs);
    
    // Try to allocate DEVICE_LOCAL memory
    uint32_t memory_type = UINT32_MAX;
    for (uint32_t i = 0; i < mem_props.memoryTypeCount; i++) {
        if ((mem_reqs.memoryTypeBits & (1 << i)) &&
            (mem_props.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) &&
            !(mem_props.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)) {
            memory_type = i;
            break;
        }
    }
    
    if (memory_type == UINT32_MAX) {
        printf("⚠️  No pure DEVICE_LOCAL memory type found\n");
        // Try DEVICE_LOCAL + HOST_VISIBLE
        for (uint32_t i = 0; i < mem_props.memoryTypeCount; i++) {
            if ((mem_reqs.memoryTypeBits & (1 << i)) &&
                (mem_props.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)) {
                memory_type = i;
                break;
            }
        }
    }
    
    printf("✅ Selected memory type %d (Heap %d)\n", memory_type, mem_props.memoryTypes[memory_type].heapIndex);
    
    // Allocate memory
    VkDeviceMemory memory;
    VkMemoryAllocateInfo alloc_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = mem_reqs.size,
        .memoryTypeIndex = memory_type
    };
    
    if (vkAllocateMemory(device, &alloc_info, NULL, &memory) != VK_SUCCESS) {
        printf("❌ Failed to allocate memory\n");
        vkDestroyBuffer(device, buffer, NULL);
        vkDestroyDevice(device, NULL);
        vkDestroyInstance(instance, NULL);
        return 1;
    }
    
    vkBindBufferMemory(device, buffer, memory, 0);
    printf("✅ Allocated 256 MB in memory type %d\n", memory_type);
    
    // Bandwidth test
    printf("\n🚀 Bandwidth Test (if accessible)...\n");
    
    // Create staging buffer if needed
    if (!(mem_props.memoryTypes[memory_type].propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)) {
        printf("📝 Memory not host visible - would need staging buffer for real test\n");
    } else {
        void* mapped;
        if (vkMapMemory(device, memory, 0, BUFFER_SIZE, 0, &mapped) == VK_SUCCESS) {
            // Simple bandwidth test
            char* test_data = malloc(BUFFER_SIZE);
            memset(test_data, 0x55, BUFFER_SIZE);
            
            double start = get_time();
            for (int i = 0; i < ITERATIONS; i++) {
                memcpy(mapped, test_data, BUFFER_SIZE);
            }
            double elapsed = get_time() - start;
            
            double bandwidth_gb = (BUFFER_SIZE / 1e9) * ITERATIONS / elapsed;
            printf("📊 Write bandwidth: %.1f GB/s\n", bandwidth_gb);
            
            if (bandwidth_gb > 50) {
                printf("🚀 This looks like VRAM bandwidth!\n");
            } else if (bandwidth_gb > 10) {
                printf("⚡ This might be PCIe bandwidth\n");
            } else {
                printf("🐌 This looks like system RAM bandwidth\n");
            }
            
            free(test_data);
            vkUnmapMemory(device, memory);
        }
    }
    
    // Cleanup
    vkFreeMemory(device, memory, NULL);
    vkDestroyBuffer(device, buffer, NULL);
    vkDestroyDevice(device, NULL);
    vkDestroyInstance(instance, NULL);
    
    printf("\n✅ Test complete!\n");
    return 0;
}