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

// IB chunk structure - try different sizes/layouts
struct drm_amdgpu_cs_chunk_ib_v1 {
    uint32_t _pad;
    uint32_t flags;
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
};

// Maybe the kernel expects raw data?
struct drm_amdgpu_cs_chunk_ib_v2 {
    uint32_t handle;     // Maybe it wants handle, not VA?
    uint32_t flags;
    uint32_t offset;     // Offset in buffer?
    uint32_t length;     // Length in bytes or dwords?
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
    
    // Write simple commands
    uint32_t *ib = (uint32_t *)ptr;
    ib[0] = 0xC0001000;
    ib[1] = 0x00000000;
    
    // First, let's see what the kernel expects by trying a minimal approach
    printf("\nTest 1: Direct handle in chunk_data\n");
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = 1;  // Maybe it just wants a handle?
    chunk.chunk_data = handle;  // Try putting handle directly
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    printf("Result: %d (%s)\n", errno, strerror(errno));
    
    // Test 2: Maybe chunk array should be inline?
    printf("\nTest 2: Try without chunk pointer indirection\n");
    // Create a structure that has chunks inline
    struct {
        uint32_t ctx_id;
        uint32_t bo_list_handle;
        uint32_t num_chunks;
        uint32_t _pad;
        struct drm_amdgpu_cs_chunk chunks[1];  // Inline chunk
    } cs_inline = {0};
    
    cs_inline.ctx_id = ctx_id;
    cs_inline.bo_list_handle = 0;
    cs_inline.num_chunks = 1;
    cs_inline.chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    cs_inline.chunks[0].length_dw = 2;
    cs_inline.chunks[0].chunk_data = handle;
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs_inline);
    printf("Result: %d (%s)\n", errno, strerror(errno));
    
    // Cleanup
    munmap(ptr, 4096);
    close(fd);
    
    printf("\nIt seems the kernel is expecting a specific format for CS submission.\n");
    printf("The EFAULT suggests the kernel can't access the memory we're pointing to.\n");
    printf("This might require understanding the exact kernel version's expectations.\n");
    
    return 0;
}