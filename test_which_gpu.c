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
    printf("=== Checking which GPU is which ===\n\n");
    
    const char* devices[] = {"/dev/dri/renderD128", "/dev/dri/renderD129"};
    const char* cards[] = {"card0", "card1"};
    const char* dri_idx[] = {"0", "1"};
    
    for (int i = 0; i < 2; i++) {
        printf("Device: %s\n", devices[i]);
        
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
            printf("  Device ID: 0x%04x", dev_info.device_id);
            if (dev_info.device_id == 0x744c) {
                printf(" (7900 XT)\n");
            } else if (dev_info.device_id == 0x164e) {
                printf(" (Raphael iGPU)\n");
            } else {
                printf(" (Unknown)\n");
            }
            printf("  Family: %d\n", dev_info.family);
            printf("  Likely card: %s\n", cards[i]);
            printf("  Likely dri index: %s\n", dri_idx[i]);
            printf("  Ring file: /sys/kernel/debug/dri/%s/amdgpu_ring_gfx_0.0.0\n", dri_idx[i]);
        }
        
        close(fd);
        printf("\n");
    }
    
    printf("Note: The mapping might be swapped!\n");
    return 0;
}