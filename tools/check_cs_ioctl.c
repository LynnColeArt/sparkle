#include <stdio.h>
#include <stdint.h>
#include <sys/ioctl.h>

// Command submission structures
struct drm_amdgpu_cs_chunk {
    uint32_t chunk_id;
    uint32_t length_dw;
    uint64_t chunk_data;
};

struct drm_amdgpu_cs_in {
    uint32_t ctx_id;
    uint32_t bo_list_handle;
    uint32_t num_chunks;
    uint32_t _pad;
    uint64_t chunks;
};

struct drm_amdgpu_cs_out {
    uint64_t handle;
};

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    struct drm_amdgpu_cs_out out;
};

// Wait idle structures
struct drm_amdgpu_gem_wait_idle_in {
    uint32_t handle;
    uint32_t flags;
    uint64_t timeout;
};

struct drm_amdgpu_gem_wait_idle_out {
    uint32_t status;
    uint32_t first_signaled;
};

union drm_amdgpu_gem_wait_idle {
    struct drm_amdgpu_gem_wait_idle_in in;
    struct drm_amdgpu_gem_wait_idle_out out;
};

#define DRM_AMDGPU_CS           0x04
#define DRM_AMDGPU_WAIT_CS      0x09
#define DRM_AMDGPU_GEM_WAIT_IDLE 0x07

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

int main() {
    printf("=== Command Submission Structures ===\n");
    printf("sizeof(struct drm_amdgpu_cs_chunk): %zu\n", sizeof(struct drm_amdgpu_cs_chunk));
    printf("sizeof(struct drm_amdgpu_cs_in): %zu\n", sizeof(struct drm_amdgpu_cs_in));
    printf("sizeof(struct drm_amdgpu_cs_out): %zu\n", sizeof(struct drm_amdgpu_cs_out));
    printf("sizeof(union drm_amdgpu_cs): %zu\n", sizeof(union drm_amdgpu_cs));
    
    printf("\nChunk structure offsets:\n");
    printf("  chunk_id offset: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk, chunk_id));
    printf("  length_dw offset: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk, length_dw));
    printf("  chunk_data offset: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk, chunk_data));
    
    printf("\nCS ioctl values:\n");
    printf("DRM_AMDGPU_CS: 0x%lx\n", (unsigned long)DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs));
    printf("DRM_AMDGPU_GEM_WAIT_IDLE: 0x%lx\n", (unsigned long)DRM_IOWR(DRM_AMDGPU_GEM_WAIT_IDLE, union drm_amdgpu_gem_wait_idle));
    
    printf("\n=== Wait Idle Structure ===\n");
    printf("sizeof(union drm_amdgpu_gem_wait_idle): %zu\n", sizeof(union drm_amdgpu_gem_wait_idle));
    
    return 0;
}