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
    printf("=== Simple Ring Availability Test ===\n\n");
    
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    // Check GFX
    struct drm_amdgpu_info info_req = {0};
    info_req.query = AMDGPU_INFO_HW_IP_INFO;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_GFX;
    info_req.query_hw_ip.ip_instance = 0;
    
    struct drm_amdgpu_info_hw_ip hw_ip = {0};
    info_req.return_pointer = (uintptr_t)&hw_ip;
    info_req.return_size = sizeof(hw_ip);
    
    printf("1. GFX Ring:\n");
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Available rings: %d\n", hw_ip.available_rings);
        printf("   IB alignment: start=%d, size=%d\n", 
               hw_ip.ib_start_alignment, hw_ip.ib_size_alignment);
    } else {
        printf("   Failed to query\n");
    }
    
    // Check COMPUTE
    printf("\n2. COMPUTE Ring:\n");
    info_req.query_hw_ip.type = AMDGPU_HW_IP_COMPUTE;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Available rings: %d\n", hw_ip.available_rings);
        printf("   IB alignment: start=%d, size=%d\n", 
               hw_ip.ib_start_alignment, hw_ip.ib_size_alignment);
    } else {
        printf("   Failed to query\n");
    }
    
    // Check accelerator status
    printf("\n3. GPU Status:\n");
    info_req.query = AMDGPU_INFO_ACCEL_WORKING;
    uint32_t accel_working = 0;
    info_req.return_pointer = (uintptr_t)&accel_working;
    info_req.return_size = sizeof(accel_working);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Accelerator working: %s\n", accel_working ? "YES" : "NO");
    }
    
    // Get device info
    printf("\n4. Device Info:\n");
    struct drm_amdgpu_info_device dev_info = {0};
    info_req.query = AMDGPU_INFO_DEV_INFO;
    info_req.return_pointer = (uintptr_t)&dev_info;
    info_req.return_size = sizeof(dev_info);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Device ID: 0x%x\n", dev_info.device_id);
        printf("   Family: %d\n", dev_info.family);
        printf("   Active CUs: %d\n", dev_info.cu_active_number);
    }
    
    close(fd);
    
    printf("\n=== Analysis ===\n");
    printf("If rings show available but packets don't execute:\n");
    printf("- Missing userspace driver initialization\n");
    printf("- Need privileged setup that Mesa/ROCm normally does\n");
    printf("- Direct PM4 submission may not be fully supported\n");
    
    return 0;
}