# Experimental Implementations

## 🧪 PLAYGROUND - BREAK THINGS HERE 🧪

This directory is for trying new ideas, optimizations, and approaches without breaking production code.

## Rules

1. **ANYTHING GOES** - Try wild ideas
2. **NO QUALITY REQUIREMENTS** - Quick and dirty is fine
3. **BENCHMARK EVERYTHING** - We need to know if it's better
4. **PROMOTE WINNERS** - If it beats reference, it can become reference

## Naming Convention

- `kernel_experiment_description.f90`
- Example: `conv2d_winograd_attempt.f90`
- Example: `matmul_avx512_test.f90`

## Current Experiments

- [ ] `conv2d_im2col_gemm.f90` - Try im2col approach
- [ ] `conv2d_winograd.f90` - Winograd convolution
- [ ] `conv2d_direct_tiled.f90` - Tiled direct convolution

## Promotion Process

If your experiment beats the reference:
1. Clean up the code
2. Document the optimizations
3. Add comprehensive tests
4. Benchmark on multiple problem sizes
5. Submit for review
6. If approved, move to reference/

## Failed Experiments

Keep failed experiments with a `_failed` suffix and a comment explaining why:
- `conv2d_recursive_failed.f90` - Too much overhead for small tiles