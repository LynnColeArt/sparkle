// Streaming (non-temporal) store wrappers for Fortran
// Provides access to cache-bypass store instructions

#include <immintrin.h>
#include <string.h>
#include <stdint.h>

// Wrapper for _mm512_stream_ps (AVX-512 non-temporal store)
void mm512_stream_ps_wrapper(float* addr, const float* data) {
    // Load the data into a vector register
    __m512 vec = _mm512_loadu_ps(data);
    
    // Stream it out, bypassing cache
    _mm512_stream_ps(addr, vec);
}

// Wrapper for _mm256_stream_ps (AVX2 non-temporal store) 
void mm256_stream_ps_wrapper(float* addr, const float* data) {
    __m256 vec = _mm256_loadu_ps(data);
    _mm256_stream_ps(addr, vec);
}

// Wrapper for _mm_stream_ps (SSE non-temporal store)
void mm128_stream_ps_wrapper(float* addr, const float* data) {
    __m128 vec = _mm_loadu_ps(data);
    _mm_stream_ps(addr, vec);
}

// Memory fence to ensure streaming stores complete
void sfence_wrapper(void) {
    _mm_sfence();
}

// Non-temporal prefetch (different from regular prefetch)
void prefetchnta_wrapper(const void* addr) {
    _mm_prefetch((const char*)addr, _MM_HINT_NTA);
}

// Utility: Check if address is aligned for streaming stores
int is_aligned_for_streaming(const void* addr, size_t alignment) {
    return ((uintptr_t)addr % alignment) == 0;
}