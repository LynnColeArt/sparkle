#include <stdio.h>
#include <sys/ioctl.h>
#include <drm/drm.h>
#include <drm/amdgpu_drm.h>

int main() {
    printf("AMDGPU ioctl values:\n");
    printf("DRM_AMDGPU_GEM_CREATE: 0x%lx\n", (unsigned long)DRM_IOCTL_AMDGPU_GEM_CREATE);
    printf("DRM_AMDGPU_GEM_MMAP:   0x%lx\n", (unsigned long)DRM_IOCTL_AMDGPU_GEM_MMAP);
    printf("DRM_AMDGPU_INFO:       0x%lx\n", (unsigned long)DRM_IOCTL_AMDGPU_INFO);
    printf("DRM_AMDGPU_CS:         0x%lx\n", (unsigned long)DRM_IOCTL_AMDGPU_CS);
    
    printf("\nStructure sizes:\n");
    printf("drm_amdgpu_gem_create:     %zu\n", sizeof(union drm_amdgpu_gem_create));
    printf("drm_amdgpu_gem_mmap:       %zu\n", sizeof(union drm_amdgpu_gem_mmap));
    printf("drm_amdgpu_info:           %zu\n", sizeof(struct drm_amdgpu_info));
    
    return 0;
}