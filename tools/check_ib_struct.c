#include <stdio.h>
#include <stdint.h>

// From kernel headers - the actual IB structure
struct drm_amdgpu_cs_chunk_ib {
    uint32_t _pad;
    uint32_t flags;
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
};

int main() {
    printf("=== IB Chunk Structure ===\n");
    printf("sizeof(struct drm_amdgpu_cs_chunk_ib): %zu\n", sizeof(struct drm_amdgpu_cs_chunk_ib));
    printf("\nOffsets:\n");
    printf("  _pad: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, _pad));
    printf("  flags: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, flags));
    printf("  va_start: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, va_start));
    printf("  ib_bytes: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ib_bytes));
    printf("  ip_type: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ip_type));
    printf("  ip_instance: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ip_instance));
    printf("  ring: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ring));
    
    return 0;
}