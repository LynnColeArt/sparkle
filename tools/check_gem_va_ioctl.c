#include <stdio.h>
#include <sys/ioctl.h>
#include <stdint.h>

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40

#define DRM_IOW(nr, type) _IOW(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define DRM_AMDGPU_GEM_VA       0x08

struct drm_amdgpu_gem_va {
    uint32_t handle;
    uint32_t _pad;
    uint32_t operation;
    uint32_t flags;
    uint64_t va_address;
    uint64_t offset_in_bo;
    uint64_t map_size;
};

int main() {
    printf("=== GEM_VA ioctl info ===\n");
    printf("sizeof(struct drm_amdgpu_gem_va): %zu\n", sizeof(struct drm_amdgpu_gem_va));
    
    unsigned long iow = DRM_IOW(DRM_AMDGPU_GEM_VA, struct drm_amdgpu_gem_va);
    unsigned long iowr = DRM_IOWR(DRM_AMDGPU_GEM_VA, struct drm_amdgpu_gem_va);
    
    printf("\nGEM_VA ioctl values:\n");
    printf("DRM_IOW:  0x%lx\n", iow);
    printf("DRM_IOWR: 0x%lx\n", iowr);
    
    return 0;
}