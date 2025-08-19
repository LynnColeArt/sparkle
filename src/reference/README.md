# Reference Implementations

## ⚠️ SACRED CODE - DO NOT MODIFY WITHOUT DISCUSSION ⚠️

This directory contains the optimized, tested, and verified reference implementations of all kernels.

## Rules

1. **NO DIRECT MODIFICATIONS** - Changes must be discussed and benchmarked
2. **PERFORMANCE DOCUMENTED** - Each implementation must document its achieved performance
3. **FULLY OPTIMIZED** - These are the best versions we have
4. **WELL COMMENTED** - Future us needs to understand the optimizations

## Current Reference Implementations

### CPU Kernels
- [ ] `conv2d_reference.f90` - Optimized convolution (TARGET: 250 GFLOPS)
- [ ] `matmul_reference.f90` - Cache-aware matrix multiplication
- [ ] `gemm_reference.f90` - BLAS-level GEMM implementation

### GPU Kernels  
- [x] `conv2d_glsl_reference.glsl` - Direct convolution shader (493 GFLOPS achieved)
- [ ] `matmul_glsl_reference.glsl` - Tiled matrix multiplication

### Memory Management
- [x] `memory_pool_reference.f90` - From sporkle_memory.f90

## How to Add a Reference Implementation

1. Implementation must be fully optimized and benchmarked
2. Must beat or match current best performance
3. Create a benchmark showing the performance
4. Document all optimizations used
5. Get review before moving to reference

## Protection

Each file should start with:
```fortran
! REFERENCE IMPLEMENTATION - DO NOT MODIFY
! Performance: XXX GFLOPS achieved on [hardware]
! Optimizations: [list key techniques used]
! Last verified: [date]
```