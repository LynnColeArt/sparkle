#include <vulkan/vulkan.h>
#include <iostream>
#include <vector>
#include <chrono>
#include <cstring>

// Simplified Vulkan memory bandwidth test
// Tests if we can get higher memory bandwidth with guaranteed device-local memory

class VulkanMemoryTest {
private:
    VkInstance instance = VK_NULL_HANDLE;
    VkPhysicalDevice physicalDevice = VK_NULL_HANDLE;
    VkDevice device = VK_NULL_HANDLE;
    VkQueue queue = VK_NULL_HANDLE;
    uint32_t queueFamilyIndex = 0;

public:
    bool initialize() {
        // Create instance
        VkApplicationInfo appInfo{};
        appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
        appInfo.pApplicationName = "Vulkan Memory Test";
        appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
        appInfo.apiVersion = VK_API_VERSION_1_0;

        VkInstanceCreateInfo createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
        createInfo.pApplicationInfo = &appInfo;

        if (vkCreateInstance(&createInfo, nullptr, &instance) != VK_SUCCESS) {
            std::cout << "❌ Failed to create Vulkan instance" << std::endl;
            return false;
        }

        // Find physical device
        uint32_t deviceCount = 0;
        vkEnumeratePhysicalDevices(instance, &deviceCount, nullptr);
        if (deviceCount == 0) {
            std::cout << "❌ No Vulkan devices found" << std::endl;
            return false;
        }

        std::vector<VkPhysicalDevice> devices(deviceCount);
        vkEnumeratePhysicalDevices(instance, &deviceCount, devices.data());

        for (const auto& dev : devices) {
            VkPhysicalDeviceProperties props;
            vkGetPhysicalDeviceProperties(dev, &props);
            std::cout << "Found GPU: " << props.deviceName << std::endl;
            
            // Check for compute queue
            uint32_t queueFamilyCount = 0;
            vkGetPhysicalDeviceQueueFamilyProperties(dev, &queueFamilyCount, nullptr);
            std::vector<VkQueueFamilyProperties> queueFamilies(queueFamilyCount);
            vkGetPhysicalDeviceQueueFamilyProperties(dev, &queueFamilyCount, queueFamilies.data());
            
            for (uint32_t i = 0; i < queueFamilyCount; i++) {
                if (queueFamilies[i].queueFlags & VK_QUEUE_TRANSFER_BIT) {
                    physicalDevice = dev;
                    queueFamilyIndex = i;
                    break;
                }
            }
            if (physicalDevice != VK_NULL_HANDLE) break;
        }

        if (physicalDevice == VK_NULL_HANDLE) {
            std::cout << "❌ No suitable Vulkan device found" << std::endl;
            return false;
        }

        // Create device
        VkDeviceQueueCreateInfo queueCreateInfo{};
        queueCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        queueCreateInfo.queueFamilyIndex = queueFamilyIndex;
        queueCreateInfo.queueCount = 1;
        float queuePriority = 1.0f;
        queueCreateInfo.pQueuePriorities = &queuePriority;

        VkDeviceCreateInfo deviceCreateInfo{};
        deviceCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
        deviceCreateInfo.pQueueCreateInfos = &queueCreateInfo;
        deviceCreateInfo.queueCreateInfoCount = 1;

        if (vkCreateDevice(physicalDevice, &deviceCreateInfo, nullptr, &device) != VK_SUCCESS) {
            std::cout << "❌ Failed to create Vulkan device" << std::endl;
            return false;
        }

        vkGetDeviceQueue(device, queueFamilyIndex, 0, &queue);
        std::cout << "✅ Vulkan initialized successfully" << std::endl;
        return true;
    }

    double testMemoryBandwidth() {
        // Test memory bandwidth with device-local vs host-visible memory
        const size_t testSize = 256 * 1024 * 1024; // 1GB
        
        std::cout << "\n🔬 Testing Vulkan memory bandwidth..." << std::endl;
        std::cout << "Test size: " << testSize / (1024 * 1024) << " MB" << std::endl;

        // Get memory properties
        VkPhysicalDeviceMemoryProperties memProps;
        vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProps);

        std::cout << "\nMemory types available:" << std::endl;
        for (uint32_t i = 0; i < memProps.memoryTypeCount; i++) {
            VkMemoryPropertyFlags flags = memProps.memoryTypes[i].propertyFlags;
            std::cout << "  Type " << i << ": ";
            if (flags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) std::cout << "DEVICE_LOCAL ";
            if (flags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) std::cout << "HOST_VISIBLE ";
            if (flags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) std::cout << "HOST_COHERENT ";
            if (flags & VK_MEMORY_PROPERTY_HOST_CACHED_BIT) std::cout << "HOST_CACHED ";
            std::cout << std::endl;
        }

        // Test 1: Host-visible memory (like OpenGL probably uses)
        double hostBandwidth = testMemoryType(testSize, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, "Host-visible");

        // Test 2: Device-local memory (true VRAM)
        double deviceBandwidth = testMemoryType(testSize, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, "Device-local");

        std::cout << "\n🎯 VULKAN MEMORY COMPARISON:" << std::endl;
        std::cout << "Host-visible bandwidth: " << hostBandwidth << " GB/s" << std::endl;
        std::cout << "Device-local bandwidth: " << deviceBandwidth << " GB/s" << std::endl;
        
        if (deviceBandwidth > hostBandwidth * 2.0) {
            std::cout << "🎉 BREAKTHROUGH! Device-local memory is " << (deviceBandwidth / hostBandwidth) << "x faster!" << std::endl;
            std::cout << "    This confirms OpenGL was using system RAM" << std::endl;
        } else {
            std::cout << "🤔 Similar performance - may indicate other bottlenecks" << std::endl;
        }

        return deviceBandwidth;
    }

private:
    uint32_t findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties) {
        VkPhysicalDeviceMemoryProperties memProperties;
        vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProperties);

        for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++) {
            if ((typeFilter & (1 << i)) && 
                (memProperties.memoryTypes[i].propertyFlags & properties) == properties) {
                return i;
            }
        }
        return UINT32_MAX;
    }

    double testMemoryType(size_t size, VkMemoryPropertyFlags properties, const char* name) {
        std::cout << "\nTesting " << name << " memory..." << std::endl;

        // Create buffer
        VkBufferCreateInfo bufferInfo{};
        bufferInfo.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
        bufferInfo.size = size;
        bufferInfo.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
        bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

        VkBuffer buffer;
        if (vkCreateBuffer(device, &bufferInfo, nullptr, &buffer) != VK_SUCCESS) {
            std::cout << "❌ Failed to create buffer" << std::endl;
            return 0.0;
        }

        // Get memory requirements
        VkMemoryRequirements memReq;
        vkGetBufferMemoryRequirements(device, buffer, &memReq);

        // Find appropriate memory type
        uint32_t memoryTypeIndex = findMemoryType(memReq.memoryTypeBits, properties);
        if (memoryTypeIndex == UINT32_MAX) {
            std::cout << "❌ Cannot find memory type with required properties" << std::endl;
            vkDestroyBuffer(device, buffer, nullptr);
            return 0.0;
        }

        // Allocate memory
        VkMemoryAllocateInfo allocInfo{};
        allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        allocInfo.allocationSize = memReq.size;
        allocInfo.memoryTypeIndex = memoryTypeIndex;

        VkDeviceMemory memory;
        if (vkAllocateMemory(device, &allocInfo, nullptr, &memory) != VK_SUCCESS) {
            std::cout << "❌ Failed to allocate memory" << std::endl;
            vkDestroyBuffer(device, buffer, nullptr);
            return 0.0;
        }

        vkBindBufferMemory(device, buffer, memory, 0);
        std::cout << "✅ Allocated " << size / (1024 * 1024) << " MB of " << name << " memory" << std::endl;

        // Test bandwidth (simplified - just allocation/access time)
        auto start = std::chrono::high_resolution_clock::now();

        // If host-visible, map and write
        if (properties & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {
            void* data;
            if (vkMapMemory(device, memory, 0, size, 0, &data) == VK_SUCCESS) {
                memset(data, 0x42, size);
                vkUnmapMemory(device, memory);
            }
        }

        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);

        double timeSeconds = duration.count() / 1.0e6;
        double bandwidth = (size / timeSeconds) / 1.0e9; // GB/s

        std::cout << "  Time: " << duration.count() / 1000.0 << " ms" << std::endl;
        std::cout << "  Bandwidth: " << bandwidth << " GB/s" << std::endl;

        // Cleanup
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);

        return bandwidth;
    }

public:
    void cleanup() {
        if (device != VK_NULL_HANDLE) {
            vkDestroyDevice(device, nullptr);
        }
        if (instance != VK_NULL_HANDLE) {
            vkDestroyInstance(instance, nullptr);
        }
    }
};

int main() {
    std::cout << "================================================================" << std::endl;
    std::cout << "🔬 VULKAN MEMORY CEILING TEST" << std::endl;
    std::cout << "================================================================" << std::endl;
    std::cout << "" << std::endl;
    std::cout << "Testing if Vulkan can achieve higher memory bandwidth than OpenGL" << std::endl;
    std::cout << "  🎯 OpenGL suspected ceiling: Limited to system RAM (~100 GB/s)" << std::endl;
    std::cout << "  🎯 Vulkan target: Device-local VRAM (~800+ GB/s)" << std::endl;
    std::cout << "" << std::endl;

    VulkanMemoryTest test;
    
    if (!test.initialize()) {
        return 1;
    }

    double bandwidth = test.testMemoryBandwidth();

    std::cout << "\n================================================================" << std::endl;
    std::cout << "🎯 FINAL RESULTS" << std::endl;
    std::cout << "================================================================" << std::endl;
    std::cout << "" << std::endl;

    if (bandwidth > 400.0) {
        std::cout << "🎉 VULKAN BREAKTHROUGH!" << std::endl;
        std::cout << "   High bandwidth confirms true VRAM access" << std::endl;
        std::cout << "   OpenGL driver was keeping data in system RAM" << std::endl;
        std::cout << "   Recommendation: Rewrite compute kernels to Vulkan" << std::endl;
    } else if (bandwidth > 150.0) {
        std::cout << "✅ VULKAN IMPROVEMENT!" << std::endl;
        std::cout << "   Better than system memory, but not VRAM peak" << std::endl;
        std::cout << "   Mixed memory allocation or other bottlenecks" << std::endl;
    } else {
        std::cout << "🤔 SAME CEILING REACHED" << std::endl;
        std::cout << "   Similar bandwidth to system memory" << std::endl;
        std::cout << "   May indicate deeper GPU driver or hardware limits" << std::endl;
    }

    test.cleanup();
    return 0;
}