#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

// AMDGPU DRM headers
#define DRM_AMDGPU_GEM_CREATE    0x00
#define DRM_AMDGPU_GEM_MMAP      0x01

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40

#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

// GEM domains
#define AMDGPU_GEM_DOMAIN_CPU   0x1
#define AMDGPU_GEM_DOMAIN_GTT   0x2
#define AMDGPU_GEM_DOMAIN_VRAM  0x4

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

union drm_amdgpu_gem_mmap {
    struct {
        uint32_t handle;
        uint32_t _pad;
    } in;
    uint64_t out;  // addr_ptr
};

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    printf("Opened device: fd=%d\n", fd);
    
    // Allocate a buffer
    union drm_amdgpu_gem_create gem_create = {0};
    gem_create.in.bo_size = 4096 * 1024;  // 4MB
    gem_create.in.alignment = 4096;
    gem_create.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    gem_create.in.domain_flags = 0;
    
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_CREATE, union drm_amdgpu_gem_create), &gem_create);
    if (ret != 0) {
        printf("GEM_CREATE failed: %d (errno: %d - %s)\n", ret, errno, strerror(errno));
        close(fd);
        return 1;
    }
    
    printf("Created buffer: handle=%u (0x%x)\n", gem_create.out.handle, gem_create.out.handle);
    
    // Try to get mmap offset
    union drm_amdgpu_gem_mmap mmap_req = {0};
    mmap_req.in.handle = gem_create.out.handle;
    mmap_req.in._pad = 0;
    
    printf("\nBefore mmap ioctl:\n");
    printf("  handle: %u (0x%x)\n", mmap_req.in.handle, mmap_req.in.handle);
    printf("  Full union as uint64: 0x%016lx\n", *(uint64_t*)&mmap_req);
    
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_GEM_MMAP, union drm_amdgpu_gem_mmap), &mmap_req);
    if (ret != 0) {
        printf("\nGEM_MMAP failed: %d (errno: %d - %s)\n", ret, errno, strerror(errno));
        
        // Try with the pre-calculated ioctl number
        printf("\nTrying with pre-calculated ioctl 0xc0086441...\n");
        mmap_req.in.handle = gem_create.out.handle;
        mmap_req.in._pad = 0;
        ret = ioctl(fd, 0xc0086441, &mmap_req);
        if (ret != 0) {
            printf("Still failed: %d (errno: %d - %s)\n", ret, errno, strerror(errno));
        } else {
            printf("Success! offset: 0x%016lx\n", mmap_req.out);
        }
    } else {
        printf("\nSuccess! mmap offset: 0x%016lx\n", mmap_req.out);
        
        // Try to actually map it
        void *ptr = mmap(NULL, 4096 * 1024, PROT_READ | PROT_WRITE, MAP_SHARED, fd, mmap_req.out);
        if (ptr == MAP_FAILED) {
            perror("mmap");
        } else {
            printf("Mapped successfully at %p\n", ptr);
            // Write and read test
            *(int*)ptr = 0x12345678;
            printf("Write/read test: 0x%x\n", *(int*)ptr);
            munmap(ptr, 4096 * 1024);
        }
    }
    
    close(fd);
    return 0;
}