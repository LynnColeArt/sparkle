#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

// Following Mini's exact flow

#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_BO_LIST       0x03
#define DRM_AMDGPU_CS            0x04
#define DRM_AMDGPU_GEM_VA        0x08

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOW(nr, type) _IOW(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// Constants
#define AMDGPU_GEM_DOMAIN_GTT      0x2
#define AMDGPU_CTX_OP_ALLOC_CTX    1
#define AMDGPU_BO_LIST_OP_CREATE   0
#define AMDGPU_CHUNK_ID_IB         0x01
#define AMDGPU_VA_OP_MAP           1
#define AMDGPU_VM_PAGE_READABLE    (1 << 1)
#define AMDGPU_VM_PAGE_WRITEABLE   (1 << 2)
#define AMDGPU_VM_PAGE_EXECUTABLE  (1 << 3)
#define AMDGPU_HW_IP_GFX          0

// All structures exactly as in kernel
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

struct drm_amdgpu_bo_list_in {
    uint32_t operation;
    uint32_t list_handle;
    uint32_t bo_number;
    uint32_t bo_info_size;
    uint64_t bo_info_ptr;
};

struct drm_amdgpu_bo_list_out {
    uint32_t list_handle;
    uint32_t _pad;
};

union drm_amdgpu_bo_list {
    struct drm_amdgpu_bo_list_in in;
    struct drm_amdgpu_bo_list_out out;
};

struct drm_amdgpu_bo_list_entry {
    uint32_t bo_handle;
    uint32_t bo_priority;
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

int main() {
    printf("=== Following Mini's exact flow ===\n\n");
    
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Step 1: Create context
    printf("1. Creating context...\n");
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx) < 0) {
        perror("create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    printf("   ✓ Context ID: %u\n", ctx_id);
    
    // Step 2: Create BO for IB
    printf("\n2. Creating BO for IB...\n");
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
    printf("   ✓ BO handle: %u\n", handle);
    
    // Step 3: Map BO into GPU VA space
    printf("\n3. Mapping BO to GPU VA...\n");
    uint64_t gpu_va = 0x400000;  // 4MB
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
    printf("   ✓ Mapped to VA: 0x%lx\n", gpu_va);
    
    // Step 4: Create BO list
    printf("\n4. Creating BO list...\n");
    struct drm_amdgpu_bo_list_entry entries[1] = {
        {.bo_handle = handle, .bo_priority = 0}
    };
    
    union drm_amdgpu_bo_list list = {0};
    list.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    list.in.bo_number = 1;
    list.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    list.in.bo_info_ptr = (uint64_t)entries;
    
    if (ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &list) < 0) {
        perror("create BO list");
        close(fd);
        return 1;
    }
    uint32_t bo_list_handle = list.out.list_handle;
    printf("   ✓ BO list handle: %u\n", bo_list_handle);
    
    // Step 5: Build CS submission
    printf("\n5. Building CS submission...\n");
    
    // Allocate all structures together
    size_t total_size = sizeof(struct drm_amdgpu_cs_chunk) + sizeof(struct drm_amdgpu_cs_chunk_ib);
    void *buffer = calloc(1, total_size);
    
    struct drm_amdgpu_cs_chunk *chunk = (struct drm_amdgpu_cs_chunk *)buffer;
    struct drm_amdgpu_cs_chunk_ib *ib_info = (struct drm_amdgpu_cs_chunk_ib *)((char *)buffer + sizeof(*chunk));
    
    // Fill IB info
    ib_info->_pad = 0;
    ib_info->flags = 0;
    ib_info->va_start = gpu_va;
    ib_info->ib_bytes = 16;  // 4 dwords
    ib_info->ip_type = AMDGPU_HW_IP_GFX;
    ib_info->ip_instance = 0;
    ib_info->ring = 0;
    
    // Fill chunk
    chunk->chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk->length_dw = sizeof(*ib_info) / 4;
    chunk->chunk_data = (uint64_t)ib_info;
    
    // Build CS
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = bo_list_handle;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)chunk;
    
    printf("   Chunk ID: %u\n", chunk->chunk_id);
    printf("   Length DW: %u\n", chunk->length_dw);
    printf("   IB VA: 0x%lx\n", ib_info->va_start);
    printf("   IB bytes: %u\n", ib_info->ib_bytes);
    printf("   IP type: %u\n", ib_info->ip_type);
    
    // Step 6: Submit
    printf("\n6. Submitting CS...\n");
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("   ✗ CS failed: %d (%s)\n", errno, strerror(errno));
        
        // Debug: Try to understand why
        printf("\n   Debug info:\n");
        printf("   - CS struct size: %zu\n", sizeof(union drm_amdgpu_cs));
        printf("   - Chunk size: %zu\n", sizeof(struct drm_amdgpu_cs_chunk));
        printf("   - IB info size: %zu\n", sizeof(struct drm_amdgpu_cs_chunk_ib));
        printf("   - Buffer ptr: %p\n", buffer);
        printf("   - Chunk ptr: %p\n", chunk);
        printf("   - IB info ptr: %p\n", ib_info);
    } else {
        printf("   ✓ CS succeeded! Handle: 0x%lx\n", cs.out.handle);
    }
    
    free(buffer);
    close(fd);
    return 0;
}