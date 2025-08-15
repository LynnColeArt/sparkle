#include <stdio.h>
#include <stdint.h>
#include <sys/ioctl.h>

// Context structures
struct drm_amdgpu_ctx_in {
    uint32_t op;
    uint32_t flags;
    uint32_t ctx_id;
    uint32_t _pad;
};

struct drm_amdgpu_ctx_out {
    union {
        uint32_t ctx_id;
        uint32_t _pad;
    } alloc;
    union {
        uint32_t flags;
        uint32_t _pad;
    } state;
};

union drm_amdgpu_ctx {
    struct drm_amdgpu_ctx_in in;
    struct drm_amdgpu_ctx_out out;
};

#define DRM_AMDGPU_CTX          0x02

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

int main() {
    printf("=== Context Structures ===\n");
    printf("sizeof(struct drm_amdgpu_ctx_in): %zu\n", sizeof(struct drm_amdgpu_ctx_in));
    printf("sizeof(struct drm_amdgpu_ctx_out): %zu\n", sizeof(struct drm_amdgpu_ctx_out));
    printf("sizeof(union drm_amdgpu_ctx): %zu\n", sizeof(union drm_amdgpu_ctx));
    
    printf("\nCTX ioctl value:\n");
    printf("DRM_AMDGPU_CTX: 0x%lx\n", (unsigned long)DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx));
    
    return 0;
}