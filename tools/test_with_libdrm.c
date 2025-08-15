// Use the actual libdrm headers
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <libdrm/amdgpu_drm.h>
#include <libdrm/drm.h>

static void *xalloc(size_t n, size_t align) {
    void *p = NULL;
    if (posix_memalign(&p, align ? align : 8, n)) return NULL;
    memset(p, 0, n);
    return p;
}

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) { perror("open"); return 1; }
    
    // Create context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    if (drmIoctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) != 0) {
        perror("ctx alloc");
        return 1;
    }
    uint32_t ctx_id = ctx_args.out.alloc.ctx_id;
    printf("Context: %u\n", ctx_id);
    
    // Allocate structures
    struct drm_amdgpu_cs_chunk_ib *ib = xalloc(sizeof(*ib), 8);
    struct drm_amdgpu_cs_chunk *chunks = xalloc(sizeof(*chunks), 8);
    
    // Fill IB
    ib->_pad = 0;
    ib->flags = 0;
    ib->va_start = 0;  // Invalid, should cause EINVAL
    ib->ib_bytes = 0;
    ib->ip_type = AMDGPU_HW_IP_GFX;
    ib->ip_instance = 0;
    ib->ring = 0;
    
    // Fill chunk
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(*ib) / 4;
    chunks[0].chunk_data = (uintptr_t)ib;
    
    // Fill CS
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = 0;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunks;
    
    printf("\nSizes:\n");
    printf("  sizeof(cs) = %zu\n", sizeof(cs_args));
    printf("  sizeof(chunk) = %zu\n", sizeof(*chunks));
    printf("  sizeof(ib) = %zu\n", sizeof(*ib));
    
    printf("\nPointers:\n");
    printf("  chunks @ %p\n", chunks);
    printf("  ib @ %p\n", ib);
    printf("  cs.chunks = 0x%lx\n", cs_args.in.chunks);
    printf("  chunk.chunk_data = 0x%lx\n", chunks[0].chunk_data);
    
    printf("\nSubmitting...\n");
    int ret = drmIoctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret != 0) {
        printf("Failed: %d (%s)\n", errno, strerror(errno));
        if (errno == EFAULT) {
            printf("Still EFAULT - kernel can't read our structures\n");
        } else if (errno == EINVAL) {
            printf("✓ EINVAL - structures are readable, params invalid\n");
        }
    } else {
        printf("Success?!\n");
    }
    
    free(ib);
    free(chunks);
    close(fd);
    return 0;
}