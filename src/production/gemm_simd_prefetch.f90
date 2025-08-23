! SIMD-Optimized GEMM with Software Prefetching
! =============================================
! Enhanced with explicit prefetch instructions for maximum memory bandwidth
!
! Key optimizations:
! - Software prefetching to hide memory latency
! - AVX-512 vectorization (16 floats per instruction)
! - Cache-aware tiling
! - Prefetch distance tuned for modern CPUs

module gemm_simd_prefetch
  use iso_c_binding
  use kinds
  use omp_lib
  implicit none
  
  private
  public :: gemm_simd_avx512_prefetch
  
  ! Prefetch hints for x86
  integer(c_int), parameter :: PREFETCH_T0 = 3  ! Prefetch to all cache levels
  integer(c_int), parameter :: PREFETCH_T1 = 2  ! Prefetch to L2 and L3
  integer(c_int), parameter :: PREFETCH_T2 = 1  ! Prefetch to L3 only
  integer(c_int), parameter :: PREFETCH_NTA = 0 ! Non-temporal (bypass cache)
  
  interface
    ! Wrapper for Intel prefetch intrinsic
    subroutine mm_prefetch(addr, hint) bind(c, name="mm_prefetch_wrapper")
      import :: c_ptr, c_int
      type(c_ptr), value :: addr
      integer(c_int), value :: hint
    end subroutine mm_prefetch
    
    ! Alternative portable prefetch
    subroutine builtin_prefetch(addr, rw, locality) bind(c, name="builtin_prefetch_wrapper")
      import :: c_ptr, c_int
      type(c_ptr), value :: addr
      integer(c_int), value :: rw       ! 0=read, 1=write
      integer(c_int), value :: locality  ! 0=no locality, 3=high locality
    end subroutine builtin_prefetch
  end interface
  
contains

  subroutine gemm_simd_avx512_prefetch(A, B, C, m, n, k, alpha, beta)
    real(sp), intent(in), target :: A(:), B(:)
    real(sp), intent(inout), target :: C(:)
    real(sp), intent(in) :: alpha, beta
    integer, intent(in) :: m, n, k
    
    integer :: i, j, kk, ii, jj, kkk
    integer :: tile_m, tile_n, tile_k
    integer :: prefetch_distance
    integer :: i_pf, j_pf, kk_pf
    real(sp) :: sum
    
    ! Tile sizes optimized for AVX-512 and cache
    tile_m = 64    ! Multiple of 16 for AVX-512
    tile_n = 64    
    tile_k = 256   ! Larger K tile for better cache reuse
    
    ! Prefetch distance - tune based on memory latency
    ! Typical values: 8-16 cache lines ahead
    prefetch_distance = 8 * 16  ! 8 cache lines * 16 floats per line
    
    ! Initialize C with beta scaling
    if (beta /= 1.0) then
      if (beta == 0.0) then
        !$omp parallel do simd
        do i = 1, m * n
          C(i) = 0.0
        end do
        !$omp end parallel do simd
      else
        !$omp parallel do simd
        do i = 1, m * n
          C(i) = beta * C(i)
        end do
        !$omp end parallel do simd
      end if
    end if
    
    ! Tiled matrix multiplication with prefetching
    !$omp parallel do collapse(2) private(i, j, kk, ii, jj, kkk, sum, i_pf, j_pf, kk_pf) &
    !$omp& schedule(dynamic, 1)
    do jj = 1, n, tile_n
      do ii = 1, m, tile_m
        
        ! Prefetch first tiles of A and B
        do i_pf = ii, min(ii + 15, m)
          call mm_prefetch(c_loc(A(i_pf)), PREFETCH_T0)
        end do
        
        ! K-loop (innermost for better cache reuse)
        do kkk = 1, k, tile_k
          
          ! Prefetch next K-tile of A
          if (kkk + tile_k <= k) then
            do i_pf = ii, min(ii + tile_m - 1, m), 16
              kk_pf = min(kkk + tile_k, k)
              call mm_prefetch(c_loc(A((kk_pf-1)*m + i_pf)), PREFETCH_T1)
            end do
          end if
          
          ! Micro-kernel with prefetching
          do j = jj, min(jj + tile_n - 1, n)
            
            ! Prefetch next column of B
            if (j + 1 <= min(jj + tile_n - 1, n)) then
              do kk_pf = kkk, min(kkk + 15, k)
                call mm_prefetch(c_loc(B(kk_pf + j*k)), PREFETCH_T0)
              end do
            end if
            
            ! Inner k-loop
            do kk = kkk, min(kkk + tile_k - 1, k)
              
              ! Prefetch ahead in A for next k iteration
              if (kk + prefetch_distance <= min(kkk + tile_k - 1, k)) then
                call mm_prefetch(c_loc(A((kk + prefetch_distance - 1)*m + ii)), PREFETCH_T0)
              end if
              
              ! Vectorized inner loop over m dimension
              !$omp simd
              do i = ii, min(ii + tile_m - 1, m)
                C((j-1)*m + i) = C((j-1)*m + i) + alpha * A((kk-1)*m + i) * B((j-1)*k + kk)
              end do
              !$omp end simd
            end do
          end do
          
        end do
      end do
    end do
    !$omp end parallel do
    
  end subroutine gemm_simd_avx512_prefetch

end module gemm_simd_prefetch