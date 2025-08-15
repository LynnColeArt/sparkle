#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

// BO list operations
#define DRM_AMDGPU_BO_LIST      0x03
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

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

int main() {
    printf("=== BO List Structures ===\n");
    printf("sizeof(struct drm_amdgpu_bo_list_in): %zu\n", sizeof(struct drm_amdgpu_bo_list_in));
    printf("sizeof(struct drm_amdgpu_bo_list_out): %zu\n", sizeof(struct drm_amdgpu_bo_list_out));
    printf("sizeof(union drm_amdgpu_bo_list): %zu\n", sizeof(union drm_amdgpu_bo_list));
    printf("sizeof(struct drm_amdgpu_bo_list_entry): %zu\n", sizeof(struct drm_amdgpu_bo_list_entry));
    
    printf("\nBO List ioctl value:\n");
    printf("DRM_AMDGPU_BO_LIST: 0x%lx\n", (unsigned long)DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list));
    
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Try to create an empty BO list
    struct drm_amdgpu_bo_list_entry entries[2] = {
        {.bo_handle = 1, .bo_priority = 0},  // Assume handle 1
        {.bo_handle = 2, .bo_priority = 0}   // Assume handle 2
    };
    
    union drm_amdgpu_bo_list list = {0};
    list.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    list.in.bo_number = 2;
    list.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    list.in.bo_info_ptr = (uint64_t)entries;
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_BO_LIST, union drm_amdgpu_bo_list), &list);
    if (ret == 0) {
        printf("\nCreated BO list handle: %u\n", list.out.list_handle);
    } else {
        printf("\nFailed to create BO list: %d (%s)\n", errno, strerror(errno));
    }
    
    close(fd);
    return 0;
}