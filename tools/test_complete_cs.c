// Complete CS test with double indirection and real PM4
#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#define DRM_IOCTL_BASE 'd'
#define DRM_COMMAND_BASE 0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)
#define DRM_IOW(nr, type) _IOW(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_GEM_MMAP      0x01
#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_BO_LIST       0x03
#define DRM_AMDGPU_CS            0x04
#define DRM_AMDGPU_GEM_VA        0x08

// Constants
#define AMDGPU_CTX_OP_ALLOC_CTX    1
#define AMDGPU_BO_LIST_OP_CREATE   0
#define AMDGPU_CHUNK_ID_IB         0x01
#define AMDGPU_VA_OP_MAP           1
#define AMDGPU_HW_IP_GFX           0
#define AMDGPU_GEM_DOMAIN_GTT      0x2
#define AMDGPU_VM_PAGE_READABLE    (1 << 1)
#define AMDGPU_VM_PAGE_WRITEABLE   (1 << 2)
#define AMDGPU_VM_PAGE_EXECUTABLE  (1 << 3)

// Structures
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

union drm_amdgpu_ctx_out {
    struct {
        uint32_t ctx_id;
        uint32_t _pad;
    } alloc;
};

union drm_amdgpu_ctx {
    struct drm_amdgpu_ctx_in in;
    union drm_amdgpu_ctx_out out;
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

union drm_amdgpu_cs_out {
    uint64_t handle;
};

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    union drm_amdgpu_cs_out out;
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

#define DRM_IOCTL_AMDGPU_GEM_CREATE DRM_IOWR(DRM_AMDGPU_GEM_CREATE, union drm_amdgpu_gem_create)
#define DRM_IOCTL_AMDGPU_GEM_MMAP   DRM_IOWR(DRM_AMDGPU_GEM_MMAP, union drm_amdgpu_gem_mmap)
#define DRM_IOCTL_AMDGPU_GEM_VA     DRM_IOW(DRM_AMDGPU_GEM_VA, struct drm_amdgpu_gem_va)
#define DRM_IOCTL_AMDGPU_CTX        DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx)
#define DRM_IOCTL_AMDGPU_BO_LIST    DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list)
#define DRM_IOCTL_AMDGPU_CS         DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs)

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    // 1. Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx) < 0) {
        perror("ctx");
        return 1;
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    printf("Context ID: %u\n", ctx_id);

    // 2. Create buffer
    union drm_amdgpu_gem_create gem = {0};
    gem.in.bo_size = 4096;
    gem.in.alignment = 4096;
    gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem) < 0) {
        perror("gem create");
        return 1;
    }
    uint32_t handle = gem.out.handle;
    printf("Buffer handle: %u\n", handle);

    // 3. Map to GPU VA
    uint64_t gpu_va = 0x400000;
    struct drm_amdgpu_gem_va va_req = {0};
    va_req.handle = handle;
    va_req.operation = AMDGPU_VA_OP_MAP;
    va_req.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_req.va_address = gpu_va;
    va_req.offset_in_bo = 0;
    va_req.map_size = 4096;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_req) < 0) {
        perror("gem va");
        return 1;
    }
    printf("Mapped to GPU VA: 0x%lx\n", gpu_va);

    // 4. Map for CPU access
    union drm_amdgpu_gem_mmap mmap_req = {0};
    mmap_req.in.handle = handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_req) < 0) {
        perror("gem mmap");
        return 1;
    }
    
    void *cpu_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, mmap_req.out.addr_ptr);
    if (cpu_ptr == MAP_FAILED) {
        perror("mmap");
        return 1;
    }
    
    // 5. Write PM4 commands
    uint32_t *pm4 = (uint32_t*)cpu_ptr;
    pm4[0] = 0xC0001000;  // NOP packet
    pm4[1] = 0x00000000;
    pm4[2] = 0xC0001000;  // NOP packet
    pm4[3] = 0x00000000;

    // 6. Create BO list
    struct drm_amdgpu_bo_list_entry entries[1] = {{handle, 0}};
    union drm_amdgpu_bo_list list = {0};
    list.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    list.in.bo_number = 1;
    list.in.bo_info_size = sizeof(entries[0]);
    list.in.bo_info_ptr = (uint64_t)entries;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &list) < 0) {
        perror("bo list");
        return 1;
    }
    uint32_t bo_list_handle = list.out.list_handle;
    printf("BO list handle: %u\n", bo_list_handle);

    // 7. Build CS with double indirection
    struct drm_amdgpu_cs_chunk_ib ib = {0};
    ib._pad = 0;
    ib.flags = 0;
    ib.va_start = gpu_va;
    ib.ib_bytes = 16;  // 4 dwords
    ib.ip_type = AMDGPU_HW_IP_GFX;
    ib.ip_instance = 0;
    ib.ring = 0;

    struct drm_amdgpu_cs_chunk chunk = {0};
    chunk.chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk.length_dw = sizeof(ib) / 4;
    chunk.chunk_data = (uint64_t)&ib;

    uint64_t chunk_array[1];
    chunk_array[0] = (uint64_t)&chunk;

    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = bo_list_handle;
    cs.in.num_chunks = 1;
    cs.in.chunks = (uint64_t)chunk_array;

    printf("\nSubmitting CS...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs);
    if (ret < 0) {
        printf("CS failed: errno=%d (%s)\n", errno, strerror(errno));
    } else {
        printf("✅ CS SUCCEEDED! Sequence: 0x%lx\n", cs.out.handle);
    }

    munmap(cpu_ptr, 4096);
    close(fd);
    return 0;
}