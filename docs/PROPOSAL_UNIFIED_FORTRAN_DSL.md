# Proposal: Unified Fortran DSL for Cross-Platform GPU Compute

## Executive Summary
Extend the Fortran GPU DSL to support Metal Shading Language (MSL) and Neural Engine, creating a truly unified interface for heterogeneous compute across AMD, Apple, and future backends.

## Motivation
We've successfully built a Fortran DSL that translates to GLSL for AMD GPUs with adaptive parameter optimization. The same approach could benefit Apple Silicon, where we previously achieved 14 TFLOPS (90% of theoretical) through manual optimization. A unified DSL would:

1. Enable write-once, run-anywhere GPU kernels
2. Apply adaptive optimization insights across all platforms
3. Simplify maintenance and development
4. Democratize high-performance compute across all hardware

## Technical Approach

### 1. Parser Extensions
Extend `sparkle_shader_parser_v2` to support multiple backends:
```fortran
type :: shader_kernel_v2
  ! ... existing fields ...
  integer :: target_backend = BACKEND_AUTO
end type
```

### 2. Backend Generators
Add new generator modules:
- `generate_msl()` - Metal Shading Language generation
- `generate_ane_pattern()` - Neural Engine pattern generation
- `generate_spirv()` - Future: Direct SPIR-V generation

### 3. Adaptive Optimization
Apply the same parameter passing optimization to Metal:
- Benchmark buffer vs constant memory vs threadgroup
- Profile different work group sizes
- Automatically select optimal strategies

### 4. Unified Kernel Syntax
```fortran
!@kernel(local_size_x=32, backend=metal, params=adaptive)
pure subroutine gemm_tiled(gid_x, gid_y, A, B, C, M, N, K)
  ! Single implementation compiles to optimal backend
end subroutine
```

## Implementation Phases

### Phase 1: MSL Generation (2 weeks)
- Add MSL generator to parser
- Map Fortran types to Metal types
- Handle Metal-specific features (threadgroup memory)

### Phase 2: Integration (1 week)
- Hook into existing sparkle_metal backend
- Ensure compatibility with Metal Performance Shaders
- Test on M1/M2/M3 hardware

### Phase 3: Neural Engine Patterns (2 weeks)
- Research ANE programming patterns
- Create pattern templates
- Map Fortran kernels to ANE operations

### Phase 4: Optimization (1 week)
- Port adaptive parameter framework
- Benchmark across devices
- Document performance characteristics

## Benefits
1. **Unified Development**: One kernel source for all platforms
2. **Performance**: Automatic optimization for each backend
3. **Maintainability**: Changes propagate to all platforms
4. **Innovation**: Easier to experiment with new approaches

## Risks and Mitigation
- **Risk**: Metal/ANE have unique features hard to abstract
- **Mitigation**: Allow backend-specific hints and extensions

- **Risk**: Performance overhead from abstraction
- **Mitigation**: Generate native code, no runtime translation

## Success Metrics
- Achieve 90%+ of hand-optimized Metal performance
- Successfully run same kernel on AMD and Apple GPUs
- Reduce kernel development time by 50%

## Resources Needed
- Access to Mac for testing (already have)
- Metal documentation (public)
- 4-6 weeks of development time

## Conclusion
This extension would make Sparkle/Sporkle the first truly unified heterogeneous compute framework, bringing us closer to the vision of democratized AI compute where code runs optimally on any available hardware.