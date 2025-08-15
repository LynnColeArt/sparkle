// Try to match kernel expectations exactly
#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

// Use exact definitions from kernel
#define DRM_IOCTL_BASE 'd'
#define DRM_COMMAND_BASE 0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define DRM_AMDGPU_CTX 0x02
#define DRM_AMDGPU_CS  0x04

#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CHUNK_ID_IB 0x01
#define AMDGPU_HW_IP_GFX 0

// Structures exactly as in kernel
typedef uint32_t __u32;
typedef uint64_t __u64;

struct drm_amdgpu_ctx_in {
    __u32 op;
    __u32 flags;
    __u32 ctx_id;
    __u32 _pad;
};

union drm_amdgpu_ctx_out {
    struct {
        __u32 ctx_id;
        __u32 _pad;
    } alloc;
};

union drm_amdgpu_ctx {
    struct drm_amdgpu_ctx_in in;
    union drm_amdgpu_ctx_out out;
};

struct drm_amdgpu_cs_chunk {
    __u32 chunk_id;
    __u32 length_dw;
    __u64 chunk_data;
};

struct drm_amdgpu_cs_in {
    __u32 ctx_id;
    __u32 bo_list_handle;
    __u32 num_chunks;
    __u32 _pad;
    __u64 chunks;
};

union drm_amdgpu_cs_out {
    __u64 handle;
};

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    union drm_amdgpu_cs_out out;
};

struct drm_amdgpu_cs_chunk_ib {
    __u32 _pad;
    __u32 flags;
    __u64 va_start;
    __u32 ib_bytes;
    __u32 ip_type;
    __u32 ip_instance;
    __u32 ring;
};

// Mini's checklist implementation
int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // 1. Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    if (ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx) < 0) {
        perror("ctx");
        close(fd);
        return 1;
    }
    __u32 ctx_id = ctx.out.alloc.ctx_id;
    
    // 2. Allocate ALL structures on heap with proper alignment
    void *memory = calloc(1, 256);  // Overallocate to be safe
    if (!memory) {
        perror("calloc");
        close(fd);
        return 1;
    }
    
    // Layout memory manually
    struct drm_amdgpu_cs_chunk *chunks = (struct drm_amdgpu_cs_chunk *)memory;
    struct drm_amdgpu_cs_chunk_ib *ib = (struct drm_amdgpu_cs_chunk_ib *)((char*)memory + 64);
    
    // 3. Fill IB info
    ib->_pad = 0;
    ib->flags = 0;
    ib->va_start = 0;
    ib->ib_bytes = 0;
    ib->ip_type = AMDGPU_HW_IP_GFX;
    ib->ip_instance = 0;
    ib->ring = 0;
    
    // 4. Fill chunk
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (__u64)(uintptr_t)ib;
    
    // 5. Fill CS
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 1;
    cs.in._pad = 0;
    cs.in.chunks = (__u64)(uintptr_t)chunks;
    
    // Mini's checklist
    printf("=== Mini's Checklist ===\n");
    printf("[%c] cs.num_chunks == 1: %u\n", 
           cs.in.num_chunks == 1 ? 'x' : ' ', cs.in.num_chunks);
    printf("[%c] cs.chunks == CPU pointer: %p\n", 
           cs.in.chunks != 0 ? 'x' : ' ', (void*)cs.in.chunks);
    printf("[%c] chunks[0].chunk_id == AMDGPU_CHUNK_ID_IB: %u\n",
           chunks[0].chunk_id == AMDGPU_CHUNK_ID_IB ? 'x' : ' ', chunks[0].chunk_id);
    printf("[%c] chunks[0].length_dw == sizeof(ib)/4: %u == %zu\n",
           chunks[0].length_dw == sizeof(*ib)/4 ? 'x' : ' ', 
           chunks[0].length_dw, sizeof(*ib)/4);
    printf("[%c] chunks[0].chunk_data == CPU pointer to IB: %p\n",
           chunks[0].chunk_data != 0 ? 'x' : ' ', (void*)chunks[0].chunk_data);
    printf("[%c] All structs zero-inited: yes\n", 'x');
    printf("[%c] No GPU VA except in ib.va_start: yes\n", 'x');
    
    // Dump requested by Mini
    printf("\n=== Dump for Mini ===\n");
    printf("sizeof(cs) = %zu\n", sizeof(cs));
    printf("sizeof(chunk) = %zu\n", sizeof(*chunks));
    printf("sizeof(ib) = %zu\n", sizeof(*ib));
    printf("\nPointers in hex:\n");
    printf("cs.chunks = 0x%lx\n", (unsigned long)cs.in.chunks);
    printf("chunks @ 0x%lx\n", (unsigned long)(uintptr_t)chunks);
    printf("chunks[0].chunk_data = 0x%lx\n", (unsigned long)chunks[0].chunk_data);
    printf("ib @ 0x%lx\n", (unsigned long)(uintptr_t)ib);
    printf("\nlength_dw = %u (0x%x)\n", chunks[0].length_dw, chunks[0].length_dw);
    
    // Try ioctl
    printf("\n=== Attempting CS ioctl ===\n");
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("Failed: errno=%d (%s)\n", errno, strerror(errno));
        if (errno == EFAULT) {
            printf("EFAULT: Pointers/sizes wrong\n");
        } else if (errno == EINVAL) {
            printf("✓ SUCCESS: Got past EFAULT! Struct wiring correct!\n");
        }
    } else {
        printf("Unexpectedly succeeded\n");
    }
    
    free(memory);
    close(fd);
    return 0;
}