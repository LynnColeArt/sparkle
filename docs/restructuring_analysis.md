# Restructuring Analysis - Still Issues!

## What We Tried
Following Mini's advice, we restructured to avoid the leading dimension issue by packing data contiguously for each tile.

## Results
- **Performance**: 1.7 GFLOPS (SLOWER than original!)
- **Accuracy**: Still 128% relative error
- **Memory**: Extra allocations per tile

## The Real Problem

Mini was absolutely right about the leading dimension issue, but fixing it revealed a deeper problem:

### Data Layout Mismatch

The im2col transformation expects data in a specific column-major order:
```
For each output location:
  For each input channel:
    For each kernel row:
      For each kernel column:
        Extract value
```

But our loop nesting might be creating the wrong layout!

### GEMM Expectations

Our GEMM expects:
- A (weights): K × input_rows in column-major
- B (im2col): input_rows × tile_cols in column-major  
- C (output): K × tile_cols in column-major

### The Bug Trail

1. **Original fused**: Wrong leading dimensions → reading garbage memory
2. **Restructured**: Correct memory access but wrong data ordering
3. **Both**: Getting wrong results but for different reasons!

## Next Questions for Mini

1. Should we verify the im2col packing order matches between fused and unfused?
2. Is the performance drop from allocation overhead or cache misses?
3. Should we use a different tiling strategy that maintains the original data order?

## Hypothesis

The unfused version works because it creates the ENTIRE im2col buffer in the correct order. The fused versions (both original and restructured) are creating partial buffers with potentially different orderings, leading to mismatched matrix multiplications.

We need to ensure the column ordering in our partial im2col buffers exactly matches what would be in the corresponding columns of the full im2col buffer.