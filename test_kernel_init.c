#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <drm/amdgpu_drm.h>
#include <sys/ioctl.h>

int main() {
    printf("=== Checking Kernel/Context Init ===\n\n");
    
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) return 1;
    
    // Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx);
    printf("Context: %u\n", ctx.out.alloc.ctx_id);
    
    // Check if context needs init
    ctx.in.op = AMDGPU_CTX_OP_QUERY_STATE2;
    ctx.in.ctx_id = ctx.out.alloc.ctx_id;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx) == 0) {
        printf("Context flags: 0x%x\n", ctx.out.state.flags);
        printf("Context hangs: %u\n", ctx.out.state.hangs);
    }
    
    close(fd);
    return 0;
}
