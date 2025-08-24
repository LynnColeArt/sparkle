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
    printf("=== Ring State Diagnosis ===\n\n");
    
    // Open device
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    // 1. Check GFX ring info
    printf("1. Checking GFX ring availability:\n");
    struct drm_amdgpu_info info_req = {0};
    info_req.query = AMDGPU_INFO_HW_IP_INFO;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_GFX;
    info_req.query_hw_ip.ip_instance = 0;
    
    struct drm_amdgpu_info_hw_ip hw_ip = {0};
    info_req.return_pointer = (uintptr_t)&hw_ip;
    info_req.return_size = sizeof(hw_ip);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Available GFX rings: %d\n", hw_ip.available_rings);
        printf("   HW IP version: %d.%d\n", hw_ip.hw_ip_version_major, hw_ip.hw_ip_version_minor);
        printf("   Capabilities: 0x%llx\n", (unsigned long long)hw_ip.capabilities_flags);
    } else {
        printf("   Failed to query GFX info\n");
    }
    
    // 2. Check compute ring info
    printf("\n2. Checking COMPUTE ring availability:\n");
    info_req.query_hw_ip.type = AMDGPU_HW_IP_COMPUTE;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Available COMPUTE rings: %d\n", hw_ip.available_rings);
        printf("   HW IP version: %d.%d\n", hw_ip.hw_ip_version_major, hw_ip.hw_ip_version_minor);
        printf("   Capabilities: 0x%llx\n", (unsigned long long)hw_ip.capabilities_flags);
    } else {
        printf("   Failed to query COMPUTE info\n");
    }
    
    // 3. Check firmware versions
    printf("\n3. Checking firmware versions:\n");
    struct drm_amdgpu_info_firmware fw_info = {0};
    info_req.query = AMDGPU_INFO_FW_VERSION;
    info_req.return_pointer = (uintptr_t)&fw_info;
    info_req.return_size = sizeof(fw_info);
    
    uint32_t fw_types[] = {
        AMDGPU_INFO_FW_GFX_ME,
        AMDGPU_INFO_FW_GFX_PFP,
        AMDGPU_INFO_FW_GFX_CE,
        AMDGPU_INFO_FW_GFX_MEC
    };
    const char* fw_names[] = {"GFX_ME", "GFX_PFP", "GFX_CE", "GFX_MEC"};
    
    for (int i = 0; i < 4; i++) {
        info_req.fw_type = fw_types[i];
        
        if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
            printf("   %s: version=0x%08x feature=0x%08x\n", 
                   fw_names[i], fw_info.ver, fw_info.feature);
        }
    }
    
    // 4. Check if we're missing initialization
    printf("\n4. Checking GPU ready state:\n");
    info_req.query = AMDGPU_INFO_ACCEL_WORKING;
    uint32_t accel_working = 0;
    info_req.return_pointer = (uintptr_t)&accel_working;
    info_req.return_size = sizeof(accel_working);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("   Accelerator working: %s\n", accel_working ? "YES" : "NO");
    }
    
    // 5. Try to understand ring requirements
    printf("\n5. Ring submission requirements:\n");
    printf("   - Kernel expects userspace (Mesa/ROCm) to do additional init\n");
    printf("   - Direct PM4 submission might need:\n");
    printf("     * Ring buffer setup\n");
    printf("     * Firmware command streams\n");
    printf("     * Context state initialization\n");
    
    // 6. Check for VRAM lost counter (proxy for resets)
    info_req.query = AMDGPU_INFO_VRAM_LOST_COUNTER;
    uint32_t vram_lost_counter = 0;
    info_req.return_pointer = (uintptr_t)&vram_lost_counter;
    info_req.return_size = sizeof(vram_lost_counter);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("\n6. VRAM lost counter: %u\n", vram_lost_counter);
        printf("   (If this increases, GPU has reset)\n");
    }
    
    close(fd);
    
    printf("\n=== Analysis ===\n");
    printf("If rings show as available but packets don't execute, the issue is likely:\n");
    printf("1. Missing userspace initialization that Mesa/ROCm normally does\n");
    printf("2. Need to enable specific features via privileged registers\n");
    printf("3. Firmware expecting initialization sequences we're not providing\n");
    
    return 0;
}