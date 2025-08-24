#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <drm/amdgpu_drm.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

int main() {
    printf("=== VA Mapping Debug Test ===\n\n");
    
    // Open device
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open");
        return 1;
    }
    
    // Create context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    printf("Context create: ret=%d, ctx_id=%u\n", ret, ctx_args.out.alloc.ctx_id);
    
    // Create BO
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    printf("BO create: ret=%d, handle=%u\n", ret, gem_args.out.handle);
    
    // Map VA with explicit error checking
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = gem_args.out.handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    printf("\nMapping VA:\n");
    printf("  handle = %u\n", va_args.handle);
    printf("  operation = %u (MAP)\n", va_args.operation);
    printf("  flags = 0x%x (R|W)\n", va_args.flags);
    printf("  va_address = 0x%llx\n", (unsigned long long)va_args.va_address);
    printf("  offset_in_bo = %llu\n", (unsigned long long)va_args.offset_in_bo);
    printf("  map_size = %llu\n", (unsigned long long)va_args.map_size);
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    printf("VA map result: ret=%d", ret);
    if (ret < 0) {
        printf(", errno=%d (%s)\n", errno, strerror(errno));
    } else {
        printf(" (SUCCESS)\n");
    }
    
    // Try to query VM info
    struct drm_amdgpu_info info_req = {0};
    struct drm_amdgpu_info_device dev_info = {0};
    
    info_req.query = AMDGPU_INFO_DEV_INFO;
    info_req.return_pointer = (uintptr_t)&dev_info;
    info_req.return_size = sizeof(dev_info);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("\nVM info:\n");
        printf("  Virtual address offset: 0x%llx\n", 
               (unsigned long long)dev_info.virtual_address_offset);
        printf("  Virtual address max: 0x%llx\n", 
               (unsigned long long)dev_info.virtual_address_max);
        printf("  Virtual address alignment: 0x%x\n", 
               dev_info.virtual_address_alignment);
    }
    
    // Try alternative VA ranges
    printf("\nTrying different VA ranges:\n");
    uint64_t test_vas[] = {
        0x400000000,     // 16GB
        0x1000000000,    // 64GB
        0x10000,         // Very low
        0x800000000,     // Original
    };
    
    for (int i = 0; i < 4; i++) {
        // First unmap if previously mapped
        if (i > 0) {
            va_args.operation = AMDGPU_VA_OP_UNMAP;
            va_args.va_address = test_vas[i-1];
            ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
        }
        
        va_args.operation = AMDGPU_VA_OP_MAP;
        va_args.va_address = test_vas[i];
        ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
        printf("  VA 0x%llx: %s\n", 
               (unsigned long long)test_vas[i],
               ret == 0 ? "SUCCESS" : strerror(errno));
    }
    
    close(fd);
    return 0;
}