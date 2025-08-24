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

// Mini's minimal test shader - writes 0xDEADBEEF to address in s[0:1]
static const uint32_t test_shader[] = {
    // s_load_dwordx2 s[0:1], s[0:1], 0x0
    0xC0060003, 0x00000000,
    // s_mov_b32 s2, 0xDEADBEEF  
    0xBE8200FF, 0xDEADBEEF,
    // s_waitcnt lgkmcnt(0)
    0xBF8CC07F,
    // s_store_dword s2, s[0:1], 0x0
    0xC0021C00, 0x00000200,
    // s_waitcnt lgkmcnt(0)
    0xBF8CC07F,
    // s_endpgm
    0xBF810000
};

int main() {
    printf("=== Mini's PM4 Debug Test ===\n\n");
    
    // Print shader bytes for verification
    printf("Shader bytes (%zu dwords):\n", sizeof(test_shader)/4);
    for (size_t i = 0; i < sizeof(test_shader)/4; i++) {
        printf("  [%02zu] 0x%08X", i, test_shader[i]);
        if (i == 0) printf(" <- s_load_dwordx2");
        else if (i == 2) printf(" <- s_mov_b32");
        else if (i == 4) printf(" <- s_waitcnt");
        else if (i == 5) printf(" <- s_store_dword");
        else if (i == 7) printf(" <- s_waitcnt");
        else if (i == 8) printf(" <- s_endpgm");
        printf("\n");
    }
    
    // Quick test of GPU detection
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        fd = open("/dev/dri/renderD128", O_RDWR);
    }
    
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    printf("\n✅ GPU device opened\n");
    
    // Get device info
    struct drm_amdgpu_info info_req = {0};
    struct drm_amdgpu_info_device dev_info = {0};
    
    info_req.query = AMDGPU_INFO_DEV_INFO;
    info_req.return_pointer = (uintptr_t)&dev_info;
    info_req.return_size = sizeof(dev_info);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("Device ID: 0x%x\n", dev_info.device_id);
        printf("Family: %d\n", dev_info.family);
        
        // Check if it's GFX10 or GFX11
        if (dev_info.family >= 143) {  // RDNA2+
            printf("Architecture: RDNA2/3 (GFX10.3+)\n");
            printf("✅ Shader encoding should be compatible\n");
        }
    }
    
    // Create GPU context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) < 0) {
        perror("Failed to create GPU context");
        close(fd);
        return 1;
    }
    
    uint32_t gpu_ctx = ctx_args.out.alloc.ctx_id;
    printf("\n✅ GPU context created: %u\n", gpu_ctx);
    
    // Create a small test buffer
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    gem_args.in.domain_flags = 0;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create buffer");
        close(fd);
        return 1;
    }
    
    printf("✅ Test buffer created: handle=%u\n", gem_args.out.handle);
    
    // Map and initialize buffer
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = gem_args.out.handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to get mmap offset");
        close(fd);
        return 1;
    }
    
    void* ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, 
                     fd, mmap_args.out.addr_ptr);
    if (ptr == MAP_FAILED) {
        perror("Failed to mmap buffer");
        close(fd);
        return 1;
    }
    
    // Initialize with pattern
    uint32_t* data = (uint32_t*)ptr;
    for (int i = 0; i < 16; i++) {
        data[i] = 0xCAFE0000 + i;
    }
    
    printf("✅ Buffer initialized with CAFE pattern\n");
    
    // Cleanup
    munmap(ptr, 4096);
    
    // Destroy context
    ctx_args.in.op = AMDGPU_CTX_OP_FREE_CTX;
    ctx_args.in.ctx_id = gpu_ctx;
    ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    
    // Close GEM handle
    struct drm_gem_close close_args = {0};
    close_args.handle = gem_args.out.handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    close(fd);
    
    printf("\n📝 Summary:\n");
    printf("- Shader is %zu bytes (%zu dwords)\n", sizeof(test_shader), sizeof(test_shader)/4);
    printf("- GPU context and buffers work\n");
    printf("- Next: Need proper PM4 packet sequence\n");
    
    return 0;
}