// Prefetch wrapper for Fortran
// Provides access to x86 prefetch intrinsics

#include <immintrin.h>

// Wrapper for _mm_prefetch that Fortran can call
void mm_prefetch_wrapper(const void* addr, int hint) {
    // Map hint values to _MM_HINT constants
    switch(hint) {
        case 0:
            _mm_prefetch((const char*)addr, _MM_HINT_NTA);
            break;
        case 1:
            _mm_prefetch((const char*)addr, _MM_HINT_T2);
            break;
        case 2:
            _mm_prefetch((const char*)addr, _MM_HINT_T1);
            break;
        case 3:
        default:
            _mm_prefetch((const char*)addr, _MM_HINT_T0);
            break;
    }
}

// Alternative using __builtin_prefetch (more portable)
void builtin_prefetch_wrapper(const void* addr, int rw, int locality) {
    // __builtin_prefetch requires compile-time constants, so we need to handle all cases
    if (rw == 0) {  // Read
        switch(locality) {
            case 0: __builtin_prefetch(addr, 0, 0); break;
            case 1: __builtin_prefetch(addr, 0, 1); break;
            case 2: __builtin_prefetch(addr, 0, 2); break;
            case 3: __builtin_prefetch(addr, 0, 3); break;
            default: __builtin_prefetch(addr, 0, 3); break;
        }
    } else {  // Write
        switch(locality) {
            case 0: __builtin_prefetch(addr, 1, 0); break;
            case 1: __builtin_prefetch(addr, 1, 1); break;
            case 2: __builtin_prefetch(addr, 1, 2); break;
            case 3: __builtin_prefetch(addr, 1, 3); break;
            default: __builtin_prefetch(addr, 1, 3); break;
        }
    }
}