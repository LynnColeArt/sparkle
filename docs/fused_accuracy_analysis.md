# Fused im2col+GEMM Accuracy Analysis

## Current Situation
We've implemented a fused im2col+GEMM convolution that achieves 14.8 GFLOPS (2.1x speedup over unfused), but we're seeing significant numerical differences compared to the reference implementation.

## Performance Results
- **Unfused (reference)**: 9.5 GFLOPS
- **Fused implementation**: 14.8 GFLOPS
- **Speedup**: 2.1x

## Accuracy Issues
- Small test cases (3x3 input) show perfect accuracy
- Large test cases (64x64x56x56) show significant differences:
  - Max difference: 0.22998E+01
  - Relative error: 88%

## Attempted Solutions
1. **Fixed GEMM indexing bug**: Changed `B((j-1)*k + kk)` to `B(kk + (j-1)*k)` for proper column-major access
2. **Corrected im2col indexing**: Fixed column-major layout in fused implementation
3. **FP16 exploration**: Lynn suggested using 16-bit math to make numerical differences more acceptable

## Key Questions for Mini
1. **Accumulation order**: In the unfused version, we do full im2col first, then GEMM. In fused, we're doing partial im2col and partial GEMM in tiles. Could the different accumulation order cause these differences?

2. **Tile boundaries**: We're processing 64 output locations at a time. Could there be edge effects at tile boundaries?

3. **Memory access patterns**: The fused version accesses input data in a different order than unfused. Could cache effects or memory aliasing be causing issues?

4. **Floating point associativity**: With different operation ordering, are we hitting FP32 precision limits differently?

## Test Cases Created
1. `test_simple_accuracy.f90` - Small controlled test with known inputs
2. `test_fp16_accuracy.f90` - Comprehensive FP16 vs FP32 comparison
3. `test_layer2_complete.f90` - Full validation suite

## Code Structure
- **Unfused path**: 
  1. Full im2col buffer creation (all patches)
  2. Single GEMM call
  
- **Fused path**:
  1. Process in tiles of 64 output locations
  2. For each tile:
     - Extract patches for those 64 locations
     - Run GEMM on partial buffer
     - Write results

## Next Steps
- Need Mini's insight on numerical stability
- Consider different tile sizes
- Investigate accumulation order effects
- Possibly implement error compensation techniques