#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_CS            0x04

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CHUNK_ID_IB      0x01

// Test if the issue is stack vs heap allocation of structures

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

struct drm_amdgpu_cs_chunk_ib {
    uint32_t _pad;
    uint32_t flags;
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
};

void test_stack_allocation(int fd, uint32_t ctx_id) {
    printf("\n=== Testing with STACK allocation ===\n");
    
    // Stack allocated structures
    struct drm_amdgpu_cs_chunk_ib ib_info = {0};
    ib_info.va_start = 0x400000;
    ib_info.ib_bytes = 16;
    ib_info.ip_type = 0;
    
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib_info) / 4;
    chunk.chunk_data = (uint64_t)&ib_info;
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    printf("Stack result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
}

void test_heap_allocation(int fd, uint32_t ctx_id) {
    printf("\n=== Testing with HEAP allocation ===\n");
    
    // Heap allocated structures
    struct drm_amdgpu_cs_chunk_ib *ib_info = malloc(sizeof(struct drm_amdgpu_cs_chunk_ib));
    memset(ib_info, 0, sizeof(*ib_info));
    ib_info->va_start = 0x400000;
    ib_info->ib_bytes = 16;
    ib_info->ip_type = 0;
    
    struct drm_amdgpu_cs_chunk *chunk = malloc(sizeof(struct drm_amdgpu_cs_chunk));
    memset(chunk, 0, sizeof(*chunk));
    chunk->chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk->length_dw = sizeof(*ib_info) / 4;
    chunk->chunk_data = (uint64_t)ib_info;
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)chunk;
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    printf("Heap result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    free(ib_info);
    free(chunk);
}

void test_single_allocation(int fd, uint32_t ctx_id) {
    printf("\n=== Testing with SINGLE contiguous allocation ===\n");
    
    // Allocate everything in one contiguous block
    size_t total_size = sizeof(struct drm_amdgpu_cs_chunk) + sizeof(struct drm_amdgpu_cs_chunk_ib);
    void *buffer = malloc(total_size);
    memset(buffer, 0, total_size);
    
    struct drm_amdgpu_cs_chunk *chunk = (struct drm_amdgpu_cs_chunk *)buffer;
    struct drm_amdgpu_cs_chunk_ib *ib_info = (struct drm_amdgpu_cs_chunk_ib *)((char *)buffer + sizeof(*chunk));
    
    ib_info->va_start = 0x400000;
    ib_info->ib_bytes = 16;
    ib_info->ip_type = 0;
    
    chunk->chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk->length_dw = sizeof(*ib_info) / 4;
    chunk->chunk_data = (uint64_t)ib_info;
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)chunk;
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    printf("Single allocation result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    free(buffer);
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
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx);
    if (ret < 0) {
        perror("create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    printf("Created context: %u\n", ctx_id);
    
    // Test different allocation methods
    test_stack_allocation(fd, ctx_id);
    test_heap_allocation(fd, ctx_id);
    test_single_allocation(fd, ctx_id);
    
    close(fd);
    return 0;
}