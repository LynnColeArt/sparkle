#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <drm/amdgpu_drm.h>
#include <sys/ioctl.h>

int main() {
    printf("=== GPU Basic Info Test ===\n\n");
    
    // Try both render nodes
    const char* devices[] = {"/dev/dri/renderD128", "/dev/dri/renderD129"};
    
    for (int i = 0; i < 2; i++) {
        printf("Testing %s:\n", devices[i]);
        
        int fd = open(devices[i], O_RDWR);
        if (fd < 0) {
            printf("  Failed to open: %s\n\n", strerror(errno));
            continue;
        }
        
        // Get device info
        struct drm_amdgpu_info info_req = {0};
        struct drm_amdgpu_info_device dev_info = {0};
        
        info_req.query = AMDGPU_INFO_DEV_INFO;
        info_req.return_pointer = (uintptr_t)&dev_info;
        info_req.return_size = sizeof(dev_info);
        
        if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
            printf("  Device ID: 0x%x\n", dev_info.device_id);
            printf("  Family: %d\n", dev_info.family);
            printf("  Active CUs: %d\n", dev_info.cu_active_number);
            printf("  GPU counter freq: %llu MHz\n", 
                   (unsigned long long)(dev_info.gpu_counter_freq / 1000));
        }
        
        // Check accelerator status
        info_req.query = AMDGPU_INFO_ACCEL_WORKING;
        uint32_t accel_working = 0;
        info_req.return_pointer = (uintptr_t)&accel_working;
        info_req.return_size = sizeof(accel_working);
        
        if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
            printf("  Accelerator working: %s\n", accel_working ? "YES" : "NO");
        }
        
        // Check GFX ring info
        info_req.query = AMDGPU_INFO_HW_IP_INFO;
        info_req.query_hw_ip.type = AMDGPU_HW_IP_GFX;
        info_req.query_hw_ip.ip_instance = 0;
        
        struct drm_amdgpu_info_hw_ip hw_ip = {0};
        info_req.return_pointer = (uintptr_t)&hw_ip;
        info_req.return_size = sizeof(hw_ip);
        
        if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
            printf("  GFX rings available: %d\n", hw_ip.available_rings);
            printf("  GFX IB alignment: start=%d, size=%d\n", 
                   hw_ip.ib_start_alignment, hw_ip.ib_size_alignment);
        }
        
        close(fd);
        printf("\n");
    }
    
    return 0;
}