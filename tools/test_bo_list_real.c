#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

// AMDGPU ioctl definitions
#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_BO_LIST       0x03
#define DRM_AMDGPU_CTX           0x02

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

// GEM domains
#define AMDGPU_GEM_DOMAIN_GTT   0x2

// Context operations
#define AMDGPU_CTX_OP_ALLOC_CTX 1

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

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Create context first
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
    
    // Create two real buffers
    uint32_t handles[2];
    for (int i = 0; i < 2; i++) {
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
        handles[i] = gem.out.handle;
        printf("Created buffer %d with handle: %u\n", i, handles[i]);
    }
    
    // Create BO list with real buffers
    struct drm_amdgpu_bo_list_entry entries[2] = {
        {.bo_handle = handles[0], .bo_priority = 0},
        {.bo_handle = handles[1], .bo_priority = 0}
    };
    
    union drm_amdgpu_bo_list list = {0};
    list.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    list.in.bo_number = 2;
    list.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    list.in.bo_info_ptr = (uint64_t)entries;
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &list);
    if (ret == 0) {
        printf("\n✅ Created BO list handle: %u\n", list.out.list_handle);
        
        // Destroy the list
        list.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
        list.in.list_handle = list.out.list_handle;
        ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &list);
    } else {
        printf("\n❌ Failed to create BO list: %d (%s)\n", errno, strerror(errno));
    }
    
    // Also test empty BO list
    union drm_amdgpu_bo_list empty_list = {0};
    empty_list.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    empty_list.in.bo_number = 0;
    empty_list.in.bo_info_size = 0;
    empty_list.in.bo_info_ptr = 0;
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &empty_list);
    if (ret == 0) {
        printf("\n✅ Created empty BO list handle: %u\n", empty_list.out.list_handle);
    } else {
        printf("\n❌ Failed to create empty BO list: %d (%s)\n", errno, strerror(errno));
    }
    
    close(fd);
    return 0;
}