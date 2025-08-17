# GEMM Redesign Strategy - Can We Do It Without Breaking Everything?

## Current GEMM Interface
```fortran
subroutine gemm_simd_avx512(A, B, C, m, n, k, alpha, beta)
  real(real32), intent(in) :: A(:), B(:), alpha, beta
  real(real32), intent(inout) :: C(:)
  integer, intent(in) :: m, n, k
```

## What's Using It
1. **universal_memory_optimization.f90** - Full matrices, works fine
2. **cpu_conv2d_fused_correct.f90** - Slices, broken due to no LDA/LDB/LDC
3. **cpu_conv2d_fused_fp16.f90** - Same issue
4. **cpu_conv2d_fused_restructured.f90** - Works around it with copies

## The Surgical Approach

### Option 1: Add Optional Parameters (Minimal Breaking)
```fortran
subroutine gemm_simd_avx512(A, B, C, m, n, k, alpha, beta, lda, ldb, ldc)
  real(real32), intent(in) :: A(:), B(:), alpha, beta
  real(real32), intent(inout) :: C(:)
  integer, intent(in) :: m, n, k
  integer, intent(in), optional :: lda, ldb, ldc  ! NEW
  
  ! Default to tight packing if not specified
  integer :: lda_use, ldb_use, ldc_use
  lda_use = merge(lda, m, present(lda))
  ldb_use = merge(ldb, k, present(ldb))
  ldc_use = merge(ldc, m, present(ldc))
```

**Impact**: ZERO! All existing calls still work, new calls can specify strides.

### Option 2: New Function, Keep Old One
```fortran
! Keep old one exactly as is
subroutine gemm_simd_avx512(A, B, C, m, n, k, alpha, beta)
  ! ... existing implementation ...
end subroutine

! New flexible version
subroutine gemm_simd_avx512_strided(A, B, C, m, n, k, alpha, beta, lda, ldb, ldc)
  ! ... new implementation with stride support ...
end subroutine
```

**Impact**: ZERO! Old code unchanged, new code uses new function.

### Option 3: Wrapper Pattern
```fortran
! Rename old one (internal use only)
subroutine gemm_simd_avx512_packed(A, B, C, m, n, k, alpha, beta)
  ! ... existing fast implementation ...
end subroutine

! New wrapper with same name
subroutine gemm_simd_avx512(A, B, C, m, n, k, alpha, beta, lda, ldb, ldc)
  integer, intent(in), optional :: lda, ldb, ldc
  
  if (present(lda) .or. present(ldb) .or. present(ldc)) then
    ! Call flexible implementation
    call gemm_simd_avx512_flexible(...)
  else
    ! Call optimized packed version
    call gemm_simd_avx512_packed(A, B, C, m, n, k, alpha, beta)
  end if
end subroutine
```

**Impact**: ZERO! Existing calls go to fast path, new calls get flexibility.

## My Recommendation: Option 1

Add optional parameters because:
1. **Zero breakage** - Fortran optional parameters are perfect for this
2. **Single code path** - Easier to maintain and optimize
3. **Performance preserved** - Can check at runtime if strides are packed
4. **Clean upgrade path** - Just add 3 parameters to fused calls

## Implementation Strategy

```fortran
! In the GEMM inner loop:
if (lda_use == m .and. ldb_use == k .and. ldc_use == m) then
  ! Fast path - use existing vectorized code
  ! All your AVX-512 optimizations intact
else
  ! Flexible path - handle arbitrary strides
  ! Still vectorized but with gather/scatter
end if
```

## The Beautiful Part

Your 196 GFLOPS achievement remains untouched for the common case (packed matrices), while fused operations get the flexibility they need!

## Next Steps
1. Add optional parameters to gemm_simd_avx512
2. Implement stride handling in inner loops
3. Update fused implementations to pass proper strides
4. Watch accuracy become perfect
5. 🎉 Celebrate with candy!