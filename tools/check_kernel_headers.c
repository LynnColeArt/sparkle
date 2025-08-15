#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>

// Let's see what the actual system headers say
// Try to include the kernel headers if available
#ifdef __has_include
  #if __has_include(<drm/amdgpu_drm.h>)
    #include <drm/amdgpu_drm.h>
    #define HAS_DRM_HEADERS 1
  #endif
#endif

#ifndef HAS_DRM_HEADERS
// If no kernel headers, use our definitions
#define DRM_AMDGPU_CS 0x04

// Based on kernel source analysis, the ACTUAL structure is:
struct drm_amdgpu_cs_chunk_ib {
    uint32_t _pad;
    uint32_t flags;  
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
};

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

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    struct {
        uint64_t handle;
    } out;
};
#endif

int main() {
    printf("=== AMDGPU Structure Sizes ===\n");
    
#ifdef HAS_DRM_HEADERS
    printf("Using system headers from <drm/amdgpu_drm.h>\n\n");
#else
    printf("Using manual definitions (no system headers found)\n\n");
#endif
    
    printf("sizeof(struct drm_amdgpu_cs_chunk_ib) = %zu\n", sizeof(struct drm_amdgpu_cs_chunk_ib));
    printf("sizeof(struct drm_amdgpu_cs_chunk) = %zu\n", sizeof(struct drm_amdgpu_cs_chunk));
    printf("sizeof(union drm_amdgpu_cs) = %zu\n", sizeof(union drm_amdgpu_cs));
    
    // Print field offsets for IB structure
    printf("\nIB structure offsets:\n");
#ifdef HAS_DRM_HEADERS
    // Use whatever fields are in the actual header
    printf("(Check header file for actual fields)\n");
#else
    printf("  _pad: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, _pad));
    printf("  flags: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, flags));
    printf("  va_start: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, va_start));
    printf("  ib_bytes: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ib_bytes));
    printf("  ip_type: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ip_type));
    printf("  ip_instance: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ip_instance));
    printf("  ring: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_chunk_ib, ring));
#endif
    
    printf("\nCS structure offsets:\n");
    printf("  ctx_id: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, ctx_id));
    printf("  bo_list_handle: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, bo_list_handle));
    printf("  num_chunks: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, num_chunks));
    printf("  chunks: %zu\n", __builtin_offsetof(struct drm_amdgpu_cs_in, chunks));
    
    // Check ioctl encoding
    printf("\nIOCTL encoding check:\n");
    unsigned long cs_ioctl = _IOWR('d', 0x40 + DRM_AMDGPU_CS, union drm_amdgpu_cs);
    printf("DRM_IOCTL_AMDGPU_CS = 0x%lx\n", cs_ioctl);
    
    return 0;
}