! SIMD-Optimized GEMM with Cache Bypass Streaming (DISABLED)
! ===========================================================
! 
! STREAMING OPTIMIZATION DISABLED - Performance Investigation Results:
! - Cache bypass streaming was 681.6% slower for GEMM
! - GEMM algorithms inherently benefit from cache locality and data reuse
! - Non-temporal stores bypass cache that GEMM actually needs
! - Streaming is correct for write-once workloads, wrong for dense matrix multiplication
!
! This module now provides regular cache-friendly SIMD GEMM.
! The streaming infrastructure remains for future write-once workloads.

module gemm_simd_streaming
  use iso_c_binding
  use kinds
  use omp_lib
  implicit none
  
  private
  public :: gemm_simd_streaming_large
  
  ! Size threshold for streaming mode (matrices larger than L3)
  integer(i64), parameter :: STREAMING_THRESHOLD = 64 * 1024 * 1024 / 4  ! 64MB in floats
  
  interface
    ! Non-temporal store intrinsic wrapper
    subroutine mm512_stream_ps(addr, data) bind(c, name="mm512_stream_ps_wrapper")
      import :: c_ptr, c_float
      type(c_ptr), value :: addr
      real(c_float), intent(in) :: data(16)  ! AVX-512 = 16 floats
    end subroutine mm512_stream_ps
    
    ! Memory fence to ensure streaming stores complete
    subroutine sfence() bind(c, name="sfence_wrapper")
    end subroutine sfence
  end interface
  
contains

  ! Streaming GEMM for very large matrices
  subroutine gemm_simd_streaming_large(A, B, C, m, n, k, alpha, beta)
    real(sp), intent(in), target :: A(:), B(:)
    real(sp), intent(inout), target :: C(:)
    real(sp), intent(in) :: alpha, beta
    integer, intent(in) :: m, n, k
    
    integer :: i, j, kk, ii, jj, kkk
    integer :: tile_m, tile_n, tile_k
    integer :: vec_len, vec_idx
    real(sp) :: sum(16)  ! AVX-512 vector
    real(sp) :: a_vec(16), b_scalar
    real(sp) :: c_vec(16)
    logical :: use_streaming
    
    ! Tile sizes optimized for streaming
    ! Smaller tiles to fit in L1/L2, process then stream out
    tile_m = 32    ! Smaller M tile for streaming
    tile_n = 512   ! Larger N tile for bandwidth
    tile_k = 64    ! Moderate K tile
    vec_len = 16   ! AVX-512 width
    
    ! DISABLED: Cache bypass streaming optimization
    ! 
    ! Investigation showed that streaming is 681.6% slower for GEMM due to data reuse.
    ! GEMM benefits from cache locality - bypassing cache hurts performance.
    ! Streaming is correct for write-once workloads, but wrong for dense matrix multiplication.
    ! 
    ! Original logic: use_streaming = (int(m, int64) * int(n, int64) >= STREAMING_THRESHOLD)
    use_streaming = .false.  ! Disabled - use regular cache-friendly algorithm
    
    ! Streaming is disabled - always use cache-friendly algorithm
    if (use_streaming) then
      print *, "🌊 Using cache-bypass streaming for large matrix"
      print '(A,I0,A,I0,A)', "   Matrix size: ", m, "×", n, " (exceeds cache)"
    else
      print *, "📦 Using regular cache-friendly algorithm"
    end if
    
    ! Initialize C with beta scaling
    if (beta /= 1.0) then
      if (beta == 0.0) then
        ! For streaming, we still need to initialize
        !$omp parallel do collapse(2)
        do j = 1, n
          do i = 1, m
            C((j-1)*m + i) = 0.0
          end do
        end do
        !$omp end parallel do
      else
        !$omp parallel do collapse(2)
        do j = 1, n
          do i = 1, m
            C((j-1)*m + i) = beta * C((j-1)*m + i)
          end do
        end do
        !$omp end parallel do
      end if
    end if
    
    ! Tiled multiplication with streaming stores
    !$omp parallel do collapse(2) private(i,j,kk,ii,jj,kkk,sum,a_vec,b_scalar,c_vec,vec_idx) &
    !$omp& schedule(dynamic,1)
    do jj = 1, n, tile_n
      do ii = 1, m, tile_m
        
        ! Process K dimension
        do kkk = 1, k, tile_k
          
          ! Micro-kernel with vectorization
          do j = jj, min(jj + tile_n - 1, n)
            do kk = kkk, min(kkk + tile_k - 1, k)
              
              b_scalar = B(kk + (j-1)*k)
              
              ! Vectorized loop over M dimension
              do i = ii, min(ii + tile_m - 1, m), vec_len
                
                ! Load vector from A
                !$omp simd
                do vec_idx = 1, min(vec_len, m - i + 1)
                  a_vec(vec_idx) = A((kk-1)*m + i + vec_idx - 1)
                end do
                !$omp end simd
                
                if (use_streaming .and. kkk == 1) then
                  ! First K iteration - initialize sum
                  !$omp simd
                  do vec_idx = 1, min(vec_len, m - i + 1)
                    sum(vec_idx) = alpha * a_vec(vec_idx) * b_scalar
                  end do
                  !$omp end simd
                else
                  ! Load current C values
                  !$omp simd
                  do vec_idx = 1, min(vec_len, m - i + 1)
                    c_vec(vec_idx) = C((j-1)*m + i + vec_idx - 1)
                  end do
                  !$omp end simd
                  
                  ! Accumulate
                  !$omp simd
                  do vec_idx = 1, min(vec_len, m - i + 1)
                    sum(vec_idx) = c_vec(vec_idx) + alpha * a_vec(vec_idx) * b_scalar
                  end do
                  !$omp end simd
                end if
                
                if (use_streaming .and. kk == min(kkk + tile_k - 1, k)) then
                  ! Last K iteration for this tile - stream out
                  ! Use non-temporal store to bypass cache
                  if (mod(i-1, vec_len) == 0 .and. i + vec_len - 1 <= m) then
                    ! Aligned vector store
                    call mm512_stream_ps(c_loc(C((j-1)*m + i)), sum)
                  else
                    ! Fallback to regular stores for partial vectors
                    !$omp simd
                    do vec_idx = 1, min(vec_len, m - i + 1)
                      C((j-1)*m + i + vec_idx - 1) = sum(vec_idx)
                    end do
                    !$omp end simd
                  end if
                else if (.not. use_streaming) then
                  ! Regular stores for small matrices
                  !$omp simd
                  do vec_idx = 1, min(vec_len, m - i + 1)
                    C((j-1)*m + i + vec_idx - 1) = sum(vec_idx)
                  end do
                  !$omp end simd
                end if
                
              end do ! i loop
            end do ! kk loop
          end do ! j loop
          
        end do ! kkk loop
      end do ! ii loop
    end do ! jj loop
    !$omp end parallel do
    
    ! Ensure all streaming stores complete
    if (use_streaming) then
      call sfence()
    end if
    
  end subroutine gemm_simd_streaming_large

end module gemm_simd_streaming