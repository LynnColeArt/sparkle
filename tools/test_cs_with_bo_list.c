#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

// AMDGPU ioctl definitions
#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_GEM_MMAP      0x01
#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_BO_LIST       0x03
#define DRM_AMDGPU_CS            0x04

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// GEM domains
#define AMDGPU_GEM_DOMAIN_GTT   0x2

// Context operations
#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CTX_OP_FREE_CTX  2

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

// Chunk IDs
#define AMDGPU_CHUNK_ID_IB      0x01

// The actual structures
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

struct drm_amdgpu_gem_mmap_in {
    uint32_t handle;
    uint32_t _pad;
};

struct drm_amdgpu_gem_mmap_out {
    uint64_t addr_ptr;
};

union drm_amdgpu_gem_mmap {
    struct drm_amdgpu_gem_mmap_in in;
    struct drm_amdgpu_gem_mmap_out out;
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

// IB chunk structure
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
    
    // Allocate IB buffer
    union drm_amdgpu_gem_create gem = {0};
    gem.in.bo_size = 4096;
    gem.in.alignment = 4096;
    gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_CREATE, union drm_amdgpu_gem_create), &gem);
    if (ret < 0) {
        perror("create buffer");
        close(fd);
        return 1;
    }
    uint32_t handle = gem.out.handle;
    printf("Created buffer handle: %u\n", handle);
    
    // Map buffer
    union drm_amdgpu_gem_mmap mmap_req = {0};
    mmap_req.in.handle = handle;
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_MMAP, union drm_amdgpu_gem_mmap), &mmap_req);
    if (ret < 0) {
        perror("get mmap offset");
        close(fd);
        return 1;
    }
    
    void *ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, mmap_req.out.addr_ptr);
    if (ptr == MAP_FAILED) {
        perror("mmap");
        close(fd);
        return 1;
    }
    
    // Write simple NOP commands
    uint32_t *ib = (uint32_t *)ptr;
    ib[0] = 0xC0001000; // PM4 NOP
    ib[1] = 0x00000000;
    ib[2] = 0xC0001000; // PM4 NOP  
    ib[3] = 0x00000000;
    
    // Create BO list with the IB buffer
    struct drm_amdgpu_bo_list_entry entries[1] = {
        {.bo_handle = handle, .bo_priority = 0}
    };
    
    union drm_amdgpu_bo_list list = {0};
    list.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    list.in.bo_number = 1;
    list.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    list.in.bo_info_ptr = (uint64_t)entries;
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &list);
    if (ret < 0) {
        perror("create BO list");
        close(fd);
        return 1;
    }
    uint32_t bo_list_handle = list.out.list_handle;
    printf("Created BO list handle: %u\n", bo_list_handle);
    
    // Create IB chunk data structure
    struct drm_amdgpu_cs_chunk_ib ib_info = {0};
    ib_info._pad = 0;
    ib_info.flags = 0;
    ib_info.va_start = mmap_req.out.addr_ptr;
    ib_info.ib_bytes = 16;  // 4 dwords * 4 bytes
    ib_info.ip_type = 0;    // GFX ring
    ib_info.ip_instance = 0;
    ib_info.ring = 0;
    
    // Try to submit with BO list
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib_info) / 4;  // Size of IB info in dwords
    chunk.chunk_data = (uint64_t)&ib_info;  // Pointer to IB info
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = bo_list_handle;  // Use the BO list
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;
    
    printf("\nSubmitting CS with BO list:\n");
    printf("  ctx_id: %u\n", cs.in.ctx_id);
    printf("  bo_list_handle: %u\n", cs.in.bo_list_handle);
    printf("  num_chunks: %u\n", cs.in.num_chunks);
    printf("  IB va_start: 0x%lx\n", ib_info.va_start);
    printf("  IB bytes: %u\n", ib_info.ib_bytes);
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("CS failed: %d (%s)\n", errno, strerror(errno));
    } else {
        printf("✅ CS succeeded! Handle: 0x%lx\n", cs.out.handle);
    }
    
    // Cleanup
    munmap(ptr, 4096);
    
    // Destroy BO list
    list.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
    list.in.list_handle = bo_list_handle;
    ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &list);
    
    // Free context
    ctx.in.op = AMDGPU_CTX_OP_FREE_CTX;
    ctx.in.ctx_id = ctx_id;
    ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx);
    
    close(fd);
    return 0;
}