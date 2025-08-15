#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

// Mini asked for a minimal working CS struct - let's create one

#define DRM_AMDGPU_CTX           0x02
#define DRM_AMDGPU_CS            0x04

#define DRM_IOCTL_BASE          'd'
#define DRM_COMMAND_BASE        0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

#define AMDGPU_CTX_OP_ALLOC_CTX 1
#define AMDGPU_CHUNK_ID_IB      0x01

// Minimal structures
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

// CS structures packed exactly as kernel expects
struct drm_amdgpu_cs_chunk {
    uint32_t chunk_id;
    uint32_t length_dw;
    uint64_t chunk_data;
} __attribute__((packed));

struct drm_amdgpu_cs_in {
    uint32_t ctx_id;
    uint32_t bo_list_handle;
    uint32_t num_chunks;
    uint32_t _pad;
    uint64_t chunks;
} __attribute__((packed));

union drm_amdgpu_cs {
    struct drm_amdgpu_cs_in in;
    struct {
        uint64_t handle;
    } out;
};

struct drm_amdgpu_cs_chunk_ib {
    uint32_t _pad;
    uint32_t flags;
    uint64_t va_start;
    uint32_t ib_bytes;
    uint32_t ip_type;
    uint32_t ip_instance;
    uint32_t ring;
} __attribute__((packed));

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Create context
    union drm_amdgpu_ctx ctx = {0};
    ctx.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOWR(DRM_AMDGPU_CTX, union drm_amdgpu_ctx), &ctx) < 0) {
        perror("create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx.out.alloc.ctx_id;
    
    printf("=== Mini's Minimal CS Mock ===\n");
    printf("Context: %u\n", ctx_id);
    
    // Allocate everything in one buffer to ensure it stays valid
    size_t total_size = sizeof(struct drm_amdgpu_cs_chunk) + sizeof(struct drm_amdgpu_cs_chunk_ib);
    uint8_t *buffer = calloc(1, total_size);
    
    // Layout: [chunk header][IB info payload]
    struct drm_amdgpu_cs_chunk *chunk = (struct drm_amdgpu_cs_chunk *)buffer;
    struct drm_amdgpu_cs_chunk_ib *ib_info = (struct drm_amdgpu_cs_chunk_ib *)(buffer + sizeof(*chunk));
    
    // Fill IB info first
    ib_info->_pad = 0;
    ib_info->flags = 0;
    ib_info->va_start = 0x400000;  // Dummy GPU VA
    ib_info->ib_bytes = 16;
    ib_info->ip_type = 0;  // GFX
    ib_info->ip_instance = 0;
    ib_info->ring = 0;
    
    // Fill chunk header
    chunk->chunk_id = AMDGPU_CHUNK_ID_IB;
    chunk->length_dw = sizeof(*ib_info) / 4;
    chunk->chunk_data = (uint64_t)ib_info;
    
    // Fill CS
    union drm_amdgpu_cs cs = {0};
    cs.in.ctx_id = ctx_id;
    cs.in.bo_list_handle = 0;
    cs.in.num_chunks = 1;
    cs.in._pad = 0;
    cs.in.chunks = (uint64_t)chunk;
    
    // Dump the exact bytes we're sending
    printf("\nCS struct (%zu bytes):\n", sizeof(cs));
    for (size_t i = 0; i < sizeof(cs); i++) {
        printf("%02x ", ((uint8_t*)&cs)[i]);
        if ((i + 1) % 16 == 0) printf("\n");
    }
    printf("\n");
    
    printf("\nChunk array at %p:\n", chunk);
    printf("First chunk (%zu bytes):\n", sizeof(*chunk));
    for (size_t i = 0; i < sizeof(*chunk); i++) {
        printf("%02x ", ((uint8_t*)chunk)[i]);
    }
    printf("\n");
    
    printf("\nIB payload at %p:\n", ib_info);
    printf("First 16 bytes of IB info:\n");
    for (size_t i = 0; i < 16; i++) {
        printf("%02x ", ((uint8_t*)ib_info)[i]);
    }
    printf("\n");
    
    // Key values Mini asked for
    printf("\n=== Key Values ===\n");
    printf("cs.num_chunks: %u\n", cs.in.num_chunks);
    printf("cs.chunks ptr: %p (0x%lx)\n", (void*)cs.in.chunks, cs.in.chunks);
    printf("chunk[0].chunk_id: %u\n", chunk->chunk_id);
    printf("chunk[0].length_dw: %u\n", chunk->length_dw);
    printf("chunk[0].chunk_data ptr: %p (0x%lx)\n", (void*)chunk->chunk_data, chunk->chunk_data);
    printf("ib_info.va_start: 0x%lx\n", ib_info->va_start);
    printf("ib_info.ib_bytes: %u\n", ib_info->ib_bytes);
    
    // Submit
    printf("\nSubmitting...\n");
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, union drm_amdgpu_cs), &cs);
    if (ret < 0) {
        printf("Result: FAILED - errno %d (%s)\n", errno, strerror(errno));
    } else {
        printf("Result: SUCCESS - handle 0x%lx\n", cs.out.handle);
    }
    
    free(buffer);
    close(fd);
    return 0;
}