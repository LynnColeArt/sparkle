#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <drm/amdgpu_drm.h>
#include <sys/ioctl.h>

// Simple PM4 test to debug wave launch issues

int main() {
    printf("=== Simple PM4 Wave Debug ===\n\n");
    
    // Open the GPU device
    int fd = open("/dev/dri/renderD129", O_RDWR);  // card0 render node
    if (fd < 0) {
        fd = open("/dev/dri/renderD128", O_RDWR);  // Try card1
    }
    
    if (fd < 0) {
        perror("Failed to open GPU device");
        return 1;
    }
    
    printf("✅ Opened GPU device\n");
    
    // Get device info
    struct drm_amdgpu_info info_req = {0};
    struct drm_amdgpu_info_device dev_info = {0};
    
    info_req.query = AMDGPU_INFO_DEV_INFO;
    info_req.return_pointer = (uintptr_t)&dev_info;
    info_req.return_size = sizeof(dev_info);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Device ID: 0x%x\n", dev_info.device_id);
        printf("   Chip rev: %d\n", dev_info.chip_rev);
        printf("   Family: %d\n", dev_info.family);
        printf("   Max engine clock: %llu MHz\n", dev_info.max_engine_clock / 1000);
    }
    
    // Check compute queue info
    struct drm_amdgpu_info_hw_ip hw_ip_info = {0};
    info_req.query = AMDGPU_INFO_HW_IP_INFO;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_COMPUTE;
    info_req.return_pointer = (uintptr_t)&hw_ip_info;
    info_req.return_size = sizeof(hw_ip_info);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("\n✅ Compute queue info:\n");
        printf("   Available rings: %d\n", hw_ip_info.available_rings);
        printf("   Version: %d.%d\n", hw_ip_info.hw_ip_version_major, 
                                      hw_ip_info.hw_ip_version_minor);
    } else {
        printf("\n❌ Failed to get compute queue info\n");
    }
    
    // Check if compute is enabled
    info_req.query = AMDGPU_INFO_HW_IP_COUNT;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_COMPUTE;
    uint32_t ip_count = 0;
    info_req.return_pointer = (uintptr_t)&ip_count;
    info_req.return_size = sizeof(ip_count);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Compute IP blocks: %d\n", ip_count);
    }
    
    printf("\nDebug hints:\n");
    printf("- If available_rings = 0, compute queues aren't initialized\n");
    printf("- Check dmesg for amdgpu errors\n");
    printf("- May need CONFIG_HSA_AMD in kernel\n");
    printf("- Try: sudo modprobe amdgpu compute_enable=1\n");
    
    close(fd);
    return 0;
}