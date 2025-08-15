// Mini's exact litmus test with correct struct layouts
#define _GNU_SOURCE
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

// Use kernel definitions directly
#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_CS            0x04

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CHUNK_ID_IB      0x01
#define AMDGPU_HW_IP_COMPUTE    1
#define AMDGPU_HW_IP_GFX        0

// CRITICAL: Use the CORRECT field order from kernel!
struct drm_amdgpu_ctx_in {
    uint32_t op;
    uint32_t flags;
    uint32_t ctx_id;
    uint32_t _pad;
};

struct drm_amdgpu_ctx_out {
    union {
        struct {
            uint32_t ctx_id;
            uint32_t _pad;
        } alloc;
    };
};

struct drm_amdgpu_ctx {
    union {
        struct drm_amdgpu_ctx_in in;
        struct drm_amdgpu_ctx_out out;
    };
};

// Mini's correct struct layout!
struct drm_amdgpu_cs_chunk_ib {
    uint32_t ip_type;       // FIRST!
    uint32_t ip_instance;   
    uint32_t ring;
    uint32_t _pad;          // Pad to align va_start
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t flags;         // LAST!
};

struct drm_amdgpu_cs_chunk {
    uint32_t chunk_id;
    uint32_t length_dw;
    uint64_t chunk_data;
};

struct drm_amdgpu_cs {
    uint32_t ctx_id;
    uint32_t bo_list_handle;
    uint32_t num_chunks;
    uint32_t _pad;
    uint64_t chunks;
};

// Compile-time sanity checks
_Static_assert(sizeof(struct drm_amdgpu_cs_chunk) == 16, "chunk size");
_Static_assert(sizeof(struct drm_amdgpu_cs_chunk_ib) == 32, "ib payload size");
_Static_assert(sizeof(struct drm_amdgpu_cs) == 24, "cs size");

static void *xalloc(size_t n, size_t align) {
    void *p = NULL;
    if (posix_memalign(&p, align ? align : 8, n)) return NULL;
    memset(p, 0, n);
    return p;
}

#define DRM_IOCTL_AMDGPU_CTX DRM_IOWR(DRM_AMDGPU_CTX, struct drm_amdgpu_ctx)
#define DRM_IOCTL_AMDGPU_CS  DRM_IOWR(DRM_AMDGPU_CS, struct drm_amdgpu_cs)

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) { perror("open card0"); return 1; }

    // 1) Create a ctx
    struct drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx) != 0) { perror("CTX alloc"); return 1; }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    printf("Got context: %u\n", ctx_id);

    // 2) Build a single IB chunk payload in CPU memory
    struct drm_amdgpu_cs_chunk_ib *ib = xalloc(sizeof(*ib), 8);
    if (!ib) { perror("alloc ib"); return 1; }

    // NOTE: We are intentionally not setting a valid GPU VA here.
    // The goal is ONLY to make the kernel *read* our payload without EFAULT.
    ib->ip_type   = AMDGPU_HW_IP_GFX; // GFX
    ib->ip_instance = 0;
    ib->ring      = 0;
    ib->va_start  = 0;       // bogus VA → expect EINVAL later, not EFAULT
    ib->ib_bytes  = 0;       // zero-length is OK for the read test
    ib->flags     = 0;

    // 3) Fill chunk header that points to the payload
    struct drm_amdgpu_cs_chunk *chunks = xalloc(sizeof(*chunks), 8);
    if (!chunks) { perror("alloc chunks"); return 1; }
    chunks[0].chunk_id   = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw  = sizeof(*ib) / 4;  // *** CRITICAL ***
    chunks[0].chunk_data = (uint64_t)(uintptr_t)ib; // CPU pointer

    // 4) Top-level CS
    struct drm_amdgpu_cs cs = {0};
    cs.ctx_id     = ctx_id;
    cs.chunks     = (uint64_t)(uintptr_t)chunks;   // CPU pointer
    cs.num_chunks = 1;

    // Print all the critical info Mini asked for
    printf("\n=== Struct Sizes ===\n");
    printf("sizeof(cs) = %zu\n", sizeof(cs));
    printf("sizeof(chunk) = %zu\n", sizeof(*chunks));
    printf("sizeof(ib) = %zu\n", sizeof(*ib));
    
    printf("\n=== Pointer Values ===\n");
    printf("cs.chunks = 0x%lx (CPU ptr to chunk array)\n", cs.chunks);
    printf("chunks[0].chunk_data = 0x%lx (CPU ptr to IB payload)\n", chunks[0].chunk_data);
    printf("ib @ 0x%lx\n", (uint64_t)(uintptr_t)ib);
    
    printf("\n=== Key Values ===\n");
    printf("length_dw = %u (0x%x) = %zu bytes\n", chunks[0].length_dw, chunks[0].length_dw, chunks[0].length_dw * 4);
    printf("num_chunks = %u\n", cs.num_chunks);
    printf("chunk_id = %u\n", chunks[0].chunk_id);

    // 5) Call ioctl — if you still get EFAULT, pointers/sizes are wrong.
    printf("\n=== Submitting CS ===\n");
    int r = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs);
    if (r != 0) {
        int e = errno;
        fprintf(stderr, "CS rc=%d errno=%d (%s)\n", r, e, strerror(e));
        if (e == EFAULT) {
            fprintf(stderr, "EFAULT: kernel could not copy_from_user(). Check:\n"
                            "  - cs.chunks CPU pointer\n"
                            "  - chunks[0].chunk_data CPU pointer\n"
                            "  - chunks[0].length_dw = sizeof(ib)/4\n"
                            "  - struct packing matches amdgpu_drm.h exactly\n");
        } else if (e == EINVAL) {
            fprintf(stderr, "✓ EINVAL: Struct wiring is CORRECT! (bad VA/params is expected)\n");
        }
    } else {
        fprintf(stderr, "CS unexpectedly succeeded (nice?)\n");
    }

    close(fd);
    return 0;
}