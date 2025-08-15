// Mini's double indirection fix!
#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#define DRM_IOCTL_BASE 'd'
#define DRM_COMMAND_BASE 0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define DRM_AMDGPU_CTX 0x02
#define DRM_AMDGPU_CS  0x04

#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CHUNK_ID_IB 0x01
#define AMDGPU_HW_IP_COMPUTE 1

// Use UNION not struct!
struct drm_amdgpu_ctx_in {
    uint32_t op;
    uint32_t flags;
    uint32_t ctx_id;
    uint32_t _pad;
};

union drm_amdgpu_ctx_out {
    struct {
        uint32_t ctx_id;
        uint32_t _pad;
    } alloc;
};

union drm_amdgpu_ctx {
    struct drm_amdgpu_ctx_in in;
    union drm_amdgpu_ctx_out out;
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
    uint64_t chunks;  // Points to array of pointers!
};

union drm_amdgpu_cs_out {
    uint64_t handle;
};

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    union drm_amdgpu_cs_out out;
};

struct drm_amdgpu_cs_chunk_ib {
    uint32_t _pad;
    uint32_t flags;
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
};

#define DRM_IOCTL_AMDGPU_CTX DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx)
#define DRM_IOCTL_AMDGPU_CS  DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs)

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) { 
        // Try render node as Mini suggested
        fd = open("/dev/dri/renderD128", O_RDWR);
        if (fd < 0) {
            perror("open");
            return 1;
        }
        printf("Using renderD128\n");
    } else {
        printf("Using card0\n");
    }

    // Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx)) { 
        perror("CTX"); 
        return 1; 
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    printf("Got context: %u\n", ctx_id);

    // Allocate structures
    struct drm_amdgpu_cs_chunk chunks[1];
    struct drm_amdgpu_cs_chunk_ib ib;
    uint64_t chunk_array[1];  // Array of pointers!

    // Setup IB
    memset(&ib, 0, sizeof(ib));
    ib._pad = 0;
    ib.flags = 0;
    ib.va_start = 0;  // Bogus VA
    ib.ib_bytes = 0;
    ib.ip_type = AMDGPU_HW_IP_COMPUTE;
    ib.ip_instance = 0;
    ib.ring = 0;

    // Setup chunk
    memset(&chunks[0], 0, sizeof(chunks[0]));
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(ib) / 4;
    chunks[0].chunk_data = (uint64_t)(uintptr_t)&ib;

    // CRITICAL: Double indirection!
    chunk_array[0] = (uint64_t)(uintptr_t)&chunks[0];

    // Setup CS using UNION
    union drm_amdgpu_cs cs;
    memset(&cs, 0, sizeof(cs));
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)(uintptr_t)chunk_array;  // Points to array of pointers!

    printf("\n=== Double Indirection Check ===\n");
    printf("cs.in.chunks = %p (points to chunk_array)\n", (void*)cs.in.chunks);
    printf("chunk_array[0] = %p (points to chunks[0])\n", (void*)chunk_array[0]);
    printf("chunks[0] @ %p\n", &chunks[0]);
    printf("chunks[0].chunk_data = %p (points to ib)\n", (void*)chunks[0].chunk_data);
    printf("ib @ %p\n", &ib);

    printf("\n=== Submitting with double indirection ===\n");
    int r = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs);
    printf("CS rc=%d errno=%d (%s)\n", r, errno, strerror(errno));
    
    if (errno == EINVAL) {
        printf("\n✅ SUCCESS! Got past EFAULT! Structures are correct!\n");
        printf("EINVAL is expected with bogus VA/params.\n");
    } else if (errno == EFAULT) {
        printf("\n❌ Still EFAULT - check header/ABI mismatch\n");
    }

    close(fd);
    return 0;
}