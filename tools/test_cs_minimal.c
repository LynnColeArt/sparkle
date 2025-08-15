#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

// Based on Mini's insights - try the absolute minimal working CS

#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_GEM_MMAP      0x01
#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_CS            0x04
#define DRM_AMDGPU_GEM_VA        0x08

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOW(nr, type) _IOW(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// Minimal structures based on kernel headers
struct drm_amdgpu_cs_in {
    uint32_t ctx_id;
    uint32_t bo_list_handle;
    uint32_t num_chunks;
    uint32_t flags;  // This might be the issue - some versions have flags instead of _pad
    uint64_t chunks;
};

struct drm_amdgpu_cs_out {
    uint64_t handle;
};

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    struct drm_amdgpu_cs_out out;
};

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Try absolute minimal CS with zero chunks (should fail with EINVAL, not EFAULT)
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = 0;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 0;
    cs.in.chunks = 0;
    
    printf("Testing minimal CS with no chunks:\n");
    printf("sizeof(union drm_amdgpu_cs): %zu\n", sizeof(union drm_amdgpu_cs));
    printf("sizeof(cs.in): %zu\n", sizeof(cs.in));
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    // Check different structure sizes
    printf("\nChecking structure layouts:\n");
    printf("offsetof(ctx_id): %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, ctx_id));
    printf("offsetof(bo_list_handle): %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, bo_list_handle));
    printf("offsetof(num_chunks): %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, num_chunks));
    printf("offsetof(flags): %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, flags));
    printf("offsetof(chunks): %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, chunks));
    
    close(fd);
    
    printf("\nIf we're getting EINVAL with 0 chunks, that's expected.\n");
    printf("If we're getting EFAULT, the structure layout is wrong.\n");
    
    return 0;
}