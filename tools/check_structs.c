#include <stdio.h>
#include <stddef.h>
#include <drm/drm.h>
#include <drm/amdgpu_drm.h>

int main() {
    printf("=== drm_amdgpu_gem_mmap ===\n");
    printf("sizeof: %zu\n", sizeof(union drm_amdgpu_gem_mmap));
    printf("handle offset: %zu\n", offsetof(struct drm_amdgpu_gem_mmap_in, handle));
    printf("_pad offset: %zu\n", offsetof(struct drm_amdgpu_gem_mmap_in, _pad));
    printf("addr_ptr offset: %zu\n", offsetof(struct drm_amdgpu_gem_mmap_out, addr_ptr));
    
    union drm_amdgpu_gem_mmap test;
    test.in.handle = 0x12345678;
    test.in._pad = 0;
    printf("\nTest values:\n");
    printf("handle: 0x%x\n", test.in.handle);
    printf("out addr_ptr would be at: %p\n", &test.out.addr_ptr);
    
    printf("\n=== drm_amdgpu_gem_create ===\n");
    printf("sizeof: %zu\n", sizeof(union drm_amdgpu_gem_create));
    printf("bo_size offset: %zu\n", offsetof(struct drm_amdgpu_gem_create_in, bo_size));
    printf("alignment offset: %zu\n", offsetof(struct drm_amdgpu_gem_create_in, alignment));
    printf("domains offset: %zu\n", offsetof(struct drm_amdgpu_gem_create_in, domains));
    printf("domain_flags offset: %zu\n", offsetof(struct drm_amdgpu_gem_create_in, domain_flags));
    
    return 0;
}