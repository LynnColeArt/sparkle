! SIMD-Optimized GEMM Implementation
! ===================================
! Properly vectorized for AVX-512 and other SIMD architectures

module gemm_simd_optimized
  use iso_fortran_env, only: real32, int32
  use omp_lib
  implicit none
  
  private
  public :: gemm_simd_avx512
  
contains

  ! SIMD-optimized GEMM targeting AVX-512 (16 floats per vector)
  subroutine gemm_simd_avx512(A, B, C, m, n, k, alpha, beta)
    real(real32), intent(in) :: A(:), B(:), alpha, beta
    real(real32), intent(inout) :: C(:)
    integer, intent(in) :: m, n, k
    
    integer :: i, j, kk
    integer :: ii, jj, kk_tile
    integer :: tile_m, tile_n, tile_k
    integer :: i_end, j_end, kk_end
    real(real32) :: sum
    
    ! Tile sizes optimized for cache hierarchy
    ! AVX-512 processes 16 floats at once
    tile_m = 64    ! Multiple of 16 for AVX-512
    tile_n = 64    
    tile_k = 256   ! Larger K tile for better cache reuse
    
    ! Initialize C with beta scaling
    if (beta /= 1.0) then
      if (beta == 0.0) then
        !$OMP PARALLEL DO SIMD
        do i = 1, m * n
          C(i) = 0.0
        end do
        !$OMP END PARALLEL DO SIMD
      else
        !$OMP PARALLEL DO SIMD
        do i = 1, m * n
          C(i) = beta * C(i)
        end do
        !$OMP END PARALLEL DO SIMD
      end if
    end if
    
    ! Triple-nested tiled loops with proper SIMD
    !$OMP PARALLEL DO COLLAPSE(2) PRIVATE(ii,jj,kk_tile,i,j,kk,i_end,j_end,kk_end,sum) SCHEDULE(DYNAMIC,1)
    do jj = 1, n, tile_n
      do ii = 1, m, tile_m
        j_end = min(jj + tile_n - 1, n)
        i_end = min(ii + tile_m - 1, m)
        
        do kk_tile = 1, k, tile_k
          kk_end = min(kk_tile + tile_k - 1, k)
          
          ! Inner loops with proper SIMD vectorization
          do j = jj, j_end
            do kk = kk_tile, kk_end
              ! This is the key: vectorize over the m dimension
              ! AVX-512 can process 16 floats at once
              !$OMP SIMD
              do i = ii, i_end
                C((j-1)*m + i) = C((j-1)*m + i) + alpha * A((kk-1)*m + i) * B(kk + (j-1)*k)
              end do
              !$OMP END SIMD
            end do
          end do
          
        end do
      end do
    end do
    !$OMP END PARALLEL DO
    
  end subroutine gemm_simd_avx512

end module gemm_simd_optimized