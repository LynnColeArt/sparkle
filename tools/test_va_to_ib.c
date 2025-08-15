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
#define DRM_AMDGPU_INFO          0x05
#define DRM_AMDGPU_GEM_VA        0x08

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOR(nr, type) _IOR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// GEM domains
#define AMDGPU_GEM_DOMAIN_GTT   0x2

// Context operations
#define AMDGPU_CTX_OP_ALLOC_CTX 1

// Chunk IDs
#define AMDGPU_CHUNK_ID_IB      0x01

// VA operations
#define AMDGPU_VA_OP_MAP        1
#define AMDGPU_VA_OP_UNMAP      2

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

struct drm_amdgpu_gem_va {
    uint32_t handle;
    uint32_t _pad;
    uint32_t operation;
    uint32_t flags;
    uint64_t va_address;
    uint64_t offset_in_bo;
    uint64_t map_size;
};

struct drm_amdgpu_info {
    uint64_t return_pointer;
    uint32_t return_size;
    uint32_t query;
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
    
    // Map buffer to GPU VA space
    struct drm_amdgpu_gem_va va_req = {0};
    va_req.handle = handle;
    va_req.operation = AMDGPU_VA_OP_MAP;
    va_req.flags = 0;
    va_req.va_address = 0x400000;  // Choose a GPU VA address
    va_req.offset_in_bo = 0;
    va_req.map_size = 4096;
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_VA, struct drm_amdgpu_gem_va), &va_req);
    if (ret < 0) {
        printf("GEM_VA mapping failed: %d (%s)\n", errno, strerror(errno));
        printf("Trying without explicit VA mapping...\n");
    } else {
        printf("Mapped buffer to GPU VA: 0x%lx\n", va_req.va_address);
    }
    
    // Map buffer for CPU access
    union drm_amdgpu_gem_mmap mmap_req = {0};
    mmap_req.in.handle = handle;
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_MMAP, union drm_amdgpu_gem_mmap), &mmap_req);
    if (ret < 0) {
        perror("get mmap offset");
        close(fd);
        return 1;
    }
    
    printf("Got mmap offset: 0x%lx\n", mmap_req.out.addr_ptr);
    
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
    
    // Create IB info
    struct drm_amdgpu_cs_chunk_ib ib_info = {0};
    ib_info._pad = 0;
    ib_info.flags = 0;
    // Try different VA values
    ib_info.va_start = 0;  // Try 0 first (might mean use handle)
    ib_info.ib_bytes = 16;
    ib_info.ip_type = 0;
    ib_info.ip_instance = 0;
    ib_info.ring = 0;
    
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib_info) / 4;
    chunk.chunk_data = (uint64_t)&ib_info;
    
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;
    
    printf("\nTrying CS with va_start=0:\n");
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("Failed: %d (%s)\n", errno, strerror(errno));
        
        // Try with the mmap offset
        printf("\nTrying CS with va_start=mmap_offset (0x%lx):\n", mmap_req.out.addr_ptr);
        ib_info.va_start = mmap_req.out.addr_ptr;
        ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
        if (ret < 0) {
            printf("Failed: %d (%s)\n", errno, strerror(errno));
            
            // Try with explicit VA if it was mapped
            if (va_req.va_address) {
                printf("\nTrying CS with va_start=explicit_va (0x%lx):\n", va_req.va_address);
                ib_info.va_start = va_req.va_address;
                ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
                if (ret < 0) {
                    printf("Failed: %d (%s)\n", errno, strerror(errno));
                } else {
                    printf("✅ CS succeeded!\n");
                }
            }
        } else {
            printf("✅ CS succeeded!\n");
        }
    } else {
        printf("✅ CS succeeded!\n");
    }
    
    // Cleanup
    munmap(ptr, 4096);
    close(fd);
    return 0;
}