#include <stdlib.h>
#include <stdint.h>

// Simple wrapper for posix_memalign with 64-byte alignment
void* posix_memalign_wrapper(size_t size_bytes) {
    void* ptr = NULL;
    int ret = posix_memalign(&ptr, 64, size_bytes);
    if (ret != 0) {
        return NULL;
    }
    return ptr;
}