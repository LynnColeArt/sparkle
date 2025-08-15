#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

// AMDGPU ioctl definitions
#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// GEM domains
#define AMDGPU_GEM_DOMAIN_GTT   0x2

// The actual structures as used by the kernel
struct drm_amdgpu_gem_create_in {
    uint64_t bo_size;
    uint64_t alignment;
    uint64_t domains;
    uint64_t domain_flags;
};

struct drm_amdgpu_gem_create_out {
    uint32_t handle;
    uint32_t _pad;
};

union drm_amdgpu_gem_create {
    struct drm_amdgpu_gem_create_in in;
    struct drm_amdgpu_gem_create_out out;
};

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Print structure info
    printf("sizeof(union drm_amdgpu_gem_create): %zu\n", sizeof(union drm_amdgpu_gem_create));
    printf("sizeof(struct drm_amdgpu_gem_create_in): %zu\n", sizeof(struct drm_amdgpu_gem_create_in));
    printf("sizeof(struct drm_amdgpu_gem_create_out): %zu\n", sizeof(struct drm_amdgpu_gem_create_out));
    
    // Initialize and create buffer
    union drm_amdgpu_gem_create gem = {0};
    gem.in.bo_size = 4096 * 1024;  // 4MB
    gem.in.alignment = 4096;
    gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    gem.in.domain_flags = 0;
    
    printf("\nBefore ioctl:\n");
    printf("  in.bo_size: %lu\n", gem.in.bo_size);
    printf("  in.alignment: %lu\n", gem.in.alignment);
    printf("  in.domains: %lu\n", gem.in.domains);
    printf("  in.domain_flags: %lu\n", gem.in.domain_flags);
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_CREATE, union drm_amdgpu_gem_create), &gem);
    
    printf("\nAfter ioctl (ret=%d):\n", ret);
    if (ret == 0) {
        printf("  out.handle: %u (0x%x)\n", gem.out.handle, gem.out.handle);
        printf("  out._pad: %u\n", gem.out._pad);
        
        // Also show raw bytes
        printf("\nRaw bytes of union (first 16):\n");
        unsigned char *p = (unsigned char*)&gem;
        for (int i = 0; i < 16; i++) {
            printf("  [%d]: 0x%02x\n", i, p[i]);
        }
    } else {
        printf("  errno: %d (%s)\n", errno, strerror(errno));
    }
    
    close(fd);
    return 0;
}