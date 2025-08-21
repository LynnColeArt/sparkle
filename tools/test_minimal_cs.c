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
#define DRM_AMDGPU_CS            0x04

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// GEM domains
#define AMDGPU_GEM_DOMAIN_GTT   0x2

// Context operations
#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CTX_OP_FREE_CTX  2

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
    
    // Create IB chunk data structure
    // The chunk_data points to an IB descriptor, not the IB itself
    // WARNING: This test uses a SIMPLIFIED/INCORRECT structure!
    // Real code should use struct drm_amdgpu_cs_chunk_ib from amdgpu_drm.h
    // This test also incorrectly uses mmap offset as VA instead of proper GPU VA
    struct {
        uint32_t handle;     // Buffer handle
        uint32_t flags;      // IB flags
        uint64_t va_start;   // Virtual address start
        uint32_t ib_bytes;   // IB size in bytes
        uint32_t ip_type;    // IP type (0 = GFX)
    } __attribute__((packed)) ib_info = {0};
    
    ib_info.handle = handle;
    ib_info.flags = 0;
    ib_info.va_start = mmap_req.out.addr_ptr;
    ib_info.ib_bytes = 16;  // 4 dwords * 4 bytes
    ib_info.ip_type = 0;    // GFX ring
    
    // Try to submit
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib_info) / 4;  // Size of IB info in dwords
    chunk.chunk_data = (uint64_t)&ib_info;  // Pointer to IB info
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;
    
    printf("\nSubmitting CS:\n");
    printf("  ctx_id: %u\n", cs.in.ctx_id);
    printf("  num_chunks: %u\n", cs.in.num_chunks);
    printf("  chunk_id: %u\n", chunk.chunk_id);
    printf("  length_dw: %u\n", chunk.length_dw);
    printf("  chunk_data: %p (IB info)\n", (void*)chunk.chunk_data);
    printf("  IB handle: %u\n", ib_info.handle);
    printf("  IB va_start: 0x%lx\n", ib_info.va_start);
    printf("  IB bytes: %u\n", ib_info.ib_bytes);
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("CS failed: %d (%s)\n", errno, strerror(errno));
        
        // The chunk data format is wrong
    } else {
        printf("CS succeeded!\n");
    }
    
    // Cleanup
    munmap(ptr, 4096);
    
    // Free context
    ctx.in.op = AMDGPU_CTX_OP_FREE_CTX;
    ctx.in.ctx_id = ctx_id;
    ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx);
    
    close(fd);
    return 0;
}