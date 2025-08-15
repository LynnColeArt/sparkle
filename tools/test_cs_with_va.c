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
#define DRM_AMDGPU_INFO          0x05
#define DRM_AMDGPU_GEM_VA        0x08

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOW(nr, type) _IOW(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// GEM domains
#define AMDGPU_GEM_DOMAIN_GTT   0x2

// Context operations
#define AMDGPU_CTX_OP_ALLOC_CTX 1

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0

// Chunk IDs
#define AMDGPU_CHUNK_ID_IB      0x01

// VA operations
#define AMDGPU_VA_OP_MAP        1
#define AMDGPU_VA_OP_UNMAP      2

// VM page flags
#define AMDGPU_VM_PAGE_READABLE     (1 << 1)
#define AMDGPU_VM_PAGE_WRITEABLE    (1 << 2)
#define AMDGPU_VM_PAGE_EXECUTABLE   (1 << 3)

// HW IP types
#define AMDGPU_HW_IP_GFX        0
#define AMDGPU_HW_IP_COMPUTE    1

// Info query types
#define AMDGPU_INFO_HW_IP_INFO  0x02

// The actual structures
struct drm_amdgpu_info {
    uint64_t return_pointer;
    uint32_t return_size;
    uint32_t query;
    union {
        struct {
            uint32_t id;
            uint32_t _pad;
        } mode_crtc;
        struct {
            uint32_t type;
            uint32_t ip_instance;
        } query_hw_ip;
    };
};

struct drm_amdgpu_info_hw_ip {
    uint32_t hw_ip_version_major;
    uint32_t hw_ip_version_minor;
    uint64_t capabilities_flags;
    uint32_t ib_start_alignment;
    uint32_t ib_size_alignment;
    uint32_t available_rings;
    uint32_t _pad;
};

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
    uint32_t length_dw;  // Length in DWORDS!
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
    uint64_t va_start;   // GPU VA, not CPU pointer!
    uint32_t ib_bytes;   // Size in bytes
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
    
    // Query HW IP info for alignment requirements
    struct drm_amdgpu_info_hw_ip hw_ip_info = {0};
    struct drm_amdgpu_info info_req = {0};
    info_req.return_pointer = (uint64_t)&hw_ip_info;
    info_req.return_size = sizeof(hw_ip_info);
    info_req.query = AMDGPU_INFO_HW_IP_INFO;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_GFX;
    info_req.query_hw_ip.ip_instance = 0;
    
    int ret = ioctl(fd, DRM_IOW(DRM_AMDGPU_INFO, struct drm_amdgpu_info), &info_req);
    if (ret == 0) {
        printf("HW IP Info:\n");
        printf("  IB start alignment: %u\n", hw_ip_info.ib_start_alignment);
        printf("  IB size alignment: %u\n", hw_ip_info.ib_size_alignment);
        printf("  Available rings: 0x%x\n", hw_ip_info.available_rings);
    }
    
    // Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx);
    if (ret < 0) {
        perror("create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    printf("Created context: %u\n", ctx_id);
    
    // Allocate IB buffer with proper alignment
    union drm_amdgpu_gem_create gem = {0};
    gem.in.bo_size = 4096;
    gem.in.alignment = 4096;  // Use alignment from hw_ip_info if needed
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
    uint64_t gpu_va = 0x400000;  // Start at 4MB in GPU VA space
    struct drm_amdgpu_gem_va va_req = {0};
    va_req.handle = handle;
    va_req.operation = AMDGPU_VA_OP_MAP;
    va_req.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_req.va_address = gpu_va;
    va_req.offset_in_bo = 0;
    va_req.map_size = 4096;
    
    ret = ioctl(fd, DRM_IOW(DRM_AMDGPU_GEM_VA, struct drm_amdgpu_gem_va), &va_req);
    if (ret < 0) {
        perror("map VA");
        close(fd);
        return 1;
    }
    printf("Mapped buffer to GPU VA: 0x%lx\n", gpu_va);
    
    // Map buffer for CPU access
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
    ib[0] = 0xC0001000; // PM4 NOP packet: (3 << 30) | (0x10 << 8) | 0
    ib[1] = 0x00000000; // NOP data
    ib[2] = 0xC0001000; // Another NOP
    ib[3] = 0x00000000; // NOP data
    
    // Create BO list
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
    ib_info.va_start = gpu_va;      // Use GPU VA, not mmap offset!
    ib_info.ib_bytes = 16;          // 4 dwords * 4 bytes
    ib_info.ip_type = AMDGPU_HW_IP_GFX;  // GFX ring
    ib_info.ip_instance = 0;
    ib_info.ring = 0;               // Ring 0
    
    // Create chunk
    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib_info) / 4;  // Length in DWORDS!
    chunk.chunk_data = (uint64_t)&ib_info;
    
    // Submit CS
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = bo_list_handle;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)&chunk;
    
    printf("\nSubmitting CS with proper VA mapping:\n");
    printf("  ctx_id: %u\n", cs.in.ctx_id);
    printf("  bo_list_handle: %u\n", cs.in.bo_list_handle);
    printf("  num_chunks: %u\n", cs.in.num_chunks);
    printf("  chunk_id: %u\n", chunk.chunk_id);
    printf("  chunk length_dw: %u (= %zu bytes)\n", chunk.length_dw, chunk.length_dw * 4);
    printf("  IB va_start: 0x%lx (GPU VA)\n", ib_info.va_start);
    printf("  IB bytes: %u\n", ib_info.ib_bytes);
    printf("  IB ip_type: %u (GFX)\n", ib_info.ip_type);
    printf("  IB ring: %u\n", ib_info.ring);
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("CS failed: %d (%s)\n", errno, strerror(errno));
    } else {
        printf("✅ CS succeeded! Handle: 0x%lx\n", cs.out.handle);
    }
    
    // Cleanup
    munmap(ptr, 4096);
    close(fd);
    return 0;
}