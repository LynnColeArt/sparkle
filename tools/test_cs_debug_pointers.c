#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

// Debug exactly what pointers we're passing per Mini's advice

#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_CS            0x04
#define DRM_AMDGPU_GEM_VA        0x08

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOW(nr, type) _IOW(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define AMDGPU_GEM_DOMAIN_GTT      0x2
#define AMDGPU_CTX_OP_ALLOC_CTX    1
#define AMDGPU_CHUNK_ID_IB         0x01
#define AMDGPU_VA_OP_MAP           1
#define AMDGPU_VM_PAGE_READABLE    (1 << 1)
#define AMDGPU_VM_PAGE_WRITEABLE   (1 << 2)
#define AMDGPU_VM_PAGE_EXECUTABLE  (1 << 3)

struct drm_amdgpu_gem_create_in {
    uint64_t bo_size;
    uint64_t alignment;
    uint64_t domains;
    uint64_t domain_flags;
};

struct drm_amdgpu_gem_create_out {
    uint32_t handle;
    uint32_t _pad;
};

union drm_amdgpu_gem_create {
    struct drm_amdgpu_gem_create_in in;
    struct drm_amdgpu_gem_create_out out;
};

struct drm_amdgpu_ctx_in {
    uint32_t op;
    uint32_t flags;
    uint32_t ctx_id;
    uint32_t _pad;
};

union drm_amdgpu_ctx {
    struct drm_amdgpu_ctx_in in;
    struct {
        union {
            uint32_t ctx_id;
            uint32_t _pad;
        } alloc;
    } out;
};

struct drm_amdgpu_gem_va {
    uint32_t handle;
    uint32_t _pad;
    uint32_t operation;
    uint32_t flags;
    uint64_t va_address;
    uint64_t offset_in_bo;
    uint64_t map_size;
};

struct drm_amdgpu_cs_chunk {
    uint32_t chunk_id;
    uint32_t length_dw;
    uint64_t chunk_data;  // CPU pointer to chunk payload!
};

struct drm_amdgpu_cs_in {
    uint32_t ctx_id;
    uint32_t bo_list_handle;
    uint32_t num_chunks;
    uint32_t _pad;
    uint64_t chunks;  // CPU pointer to chunk array!
};

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    struct {
        uint64_t handle;
    } out;
};

struct drm_amdgpu_cs_chunk_ib {
    uint32_t _pad;
    uint32_t flags;
    uint64_t va_start;    // GPU VA is OK here!
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
};

void hexdump(const char *desc, void *addr, int len) {
    int i;
    unsigned char buff[17];
    unsigned char *pc = (unsigned char*)addr;
    
    printf("%s:\n", desc);
    for (i = 0; i < len; i++) {
        if ((i % 16) == 0) {
            if (i != 0)
                printf("  %s\n", buff);
            printf("  %04x ", i);
        }
        printf(" %02x", pc[i]);
        if ((pc[i] < 0x20) || (pc[i] > 0x7e))
            buff[i % 16] = '.';
        else
            buff[i % 16] = pc[i];
        buff[(i % 16) + 1] = '\0';
    }
    while ((i % 16) != 0) {
        printf("   ");
        i++;
    }
    printf("  %s\n", buff);
}

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx) < 0) {
        perror("create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    
    // Create and map a buffer
    union drm_amdgpu_gem_create gem = {0};
    gem.in.bo_size = 4096;
    gem.in.alignment = 4096;
    gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    if (ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_CREATE, union drm_amdgpu_gem_create), &gem) < 0) {
        perror("create buffer");
        close(fd);
        return 1;
    }
    uint32_t handle = gem.out.handle;
    
    // Map to GPU VA
    uint64_t gpu_va = 0x400000;
    struct drm_amdgpu_gem_va va_req = {0};
    va_req.handle = handle;
    va_req.operation = AMDGPU_VA_OP_MAP;
    va_req.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_req.va_address = gpu_va;
    va_req.offset_in_bo = 0;
    va_req.map_size = 4096;
    
    if (ioctl(fd, DRM_IOW(DRM_AMDGPU_GEM_VA, struct drm_amdgpu_gem_va), &va_req) < 0) {
        perror("map VA");
        close(fd);
        return 1;
    }
    
    printf("=== Setup Complete ===\n");
    printf("Context ID: %u\n", ctx_id);
    printf("BO handle: %u\n", handle);
    printf("GPU VA: 0x%lx\n", gpu_va);
    
    // Build CS structures
    printf("\n=== Building CS Structures ===\n");
    
    // IB chunk payload - this describes the IB
    struct drm_amdgpu_cs_chunk_ib ib_info = {0};
    ib_info._pad = 0;
    ib_info.flags = 0;
    ib_info.va_start = gpu_va;  // GPU VA is correct here!
    ib_info.ib_bytes = 16;
    ib_info.ip_type = 0;  // GFX
    ib_info.ip_instance = 0;
    ib_info.ring = 0;
    
    printf("IB chunk payload at %p:\n", &ib_info);
    printf("  va_start: 0x%lx (GPU VA)\n", ib_info.va_start);
    printf("  ib_bytes: %u\n", ib_info.ib_bytes);
    printf("  ip_type: %u\n", ib_info.ip_type);
    hexdump("IB chunk payload raw", &ib_info, sizeof(ib_info));
    
    // Chunk header - points to IB chunk payload
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib_info) / 4;  // Size in dwords
    chunk.chunk_data = (uint64_t)&ib_info;  // CPU pointer to payload!
    
    printf("\nChunk header at %p:\n", &chunk);
    printf("  chunk_id: %u\n", chunk.chunk_id);
    printf("  length_dw: %u (= %zu bytes)\n", chunk.length_dw, chunk.length_dw * 4);
    printf("  chunk_data: %p (CPU pointer to payload)\n", (void*)chunk.chunk_data);
    hexdump("Chunk header raw", &chunk, sizeof(chunk));
    
    // CS submission - points to chunk array
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;  // No BO list for now
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;  // CPU pointer to chunk array!
    
    printf("\nCS submission at %p:\n", &cs);
    printf("  ctx_id: %u\n", cs.in.ctx_id);
    printf("  bo_list_handle: %u\n", cs.in.bo_list_handle);
    printf("  num_chunks: %u\n", cs.in.num_chunks);
    printf("  chunks: %p (CPU pointer to chunk array)\n", (void*)cs.in.chunks);
    hexdump("CS submission raw", &cs, sizeof(cs));
    
    // Sanity check all pointers
    printf("\n=== Pointer Sanity Check ===\n");
    printf("Stack range (approx): %p - %p\n", &fd, &cs + 1);
    printf("IB info ptr: %p %s\n", &ib_info, 
           (&ib_info >= (void*)&fd && &ib_info <= (void*)(&cs + 1)) ? "(on stack ✓)" : "(NOT on stack!)");
    printf("Chunk ptr: %p %s\n", &chunk,
           (&chunk >= (void*)&fd && &chunk <= (void*)(&cs + 1)) ? "(on stack ✓)" : "(NOT on stack!)");
    printf("CS ptr: %p %s\n", &cs,
           (&cs >= (void*)&fd && &cs <= (void*)(&cs + 1)) ? "(on stack ✓)" : "(NOT on stack!)");
    
    // Check for suspicious values
    if (chunk.chunk_data == gpu_va) {
        printf("\n⚠️  WARNING: chunk_data == gpu_va! This should be a CPU pointer!\n");
    }
    if (cs.in.chunks == gpu_va) {
        printf("\n⚠️  WARNING: cs.chunks == gpu_va! This should be a CPU pointer!\n");
    }
    
    // Submit
    printf("\n=== Submitting CS ===\n");
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("CS failed: %d (%s)\n", errno, strerror(errno));
    } else {
        printf("CS succeeded! Handle: 0x%lx\n", cs.out.handle);
    }
    
    close(fd);
    return 0;
}