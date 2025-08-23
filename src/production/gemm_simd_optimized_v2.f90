! SIMD-Optimized GEMM Implementation V2 - With Leading Dimension Support
! =====================================================================
! Following Mini's recommendations for surgical, robust changes

module gemm_simd_optimized_v2
  use kinds
  use omp_lib
  implicit none
  
  private
  public :: gemm_simd_avx512_v2
  
contains

  ! SIMD-optimized GEMM with optional leading dimensions
  subroutine gemm_simd_avx512_v2(A, B, C, m, n, k, alpha, beta, lda, ldb, ldc)
    real(sp), intent(in) :: A(:), B(:), alpha, beta
    real(sp), intent(inout) :: C(:)
    integer, intent(in) :: m, n, k
    integer, intent(in), optional :: lda, ldb, ldc
    
    ! Local variables
    integer :: lda_use, ldb_use, ldc_use
    integer :: i, j, kk, ii, jj, kkk
    integer :: tile_m, tile_n, tile_k
    real(sp) :: sum
    
    ! Following Mini's advice - proper optional handling
    if (present(lda)) then
      lda_use = lda
    else
      lda_use = m
    end if
    
    if (present(ldb)) then
      ldb_use = ldb
    else
      ldb_use = k
    end if
    
    if (present(ldc)) then
      ldc_use = ldc
    else
      ldc_use = m
    end if
    
    ! Validate leading dimensions (debug builds)
    if (lda_use < max(1, m)) then
      error stop "gemm_simd_avx512_v2: lda too small"
    end if
    if (ldb_use < max(1, k)) then
      error stop "gemm_simd_avx512_v2: ldb too small"
    end if
    if (ldc_use < max(1, m)) then
      error stop "gemm_simd_avx512_v2: ldc too small"
    end if
    
    ! Check if we can use the fast packed path
    if (lda_use == m .and. ldb_use == k .and. ldc_use == m) then
      ! Fast packed path - original implementation
      call gemm_simd_avx512_packed(A, B, C, m, n, k, alpha, beta)
    else
      ! Flexible strided path
      call gemm_simd_avx512_strided(A, B, C, m, n, k, alpha, beta, lda_use, ldb_use, ldc_use)
    end if
    
  end subroutine gemm_simd_avx512_v2
  
  ! Fast packed implementation (original code)
  subroutine gemm_simd_avx512_packed(A, B, C, m, n, k, alpha, beta)
    real(sp), intent(in) :: A(:), B(:), alpha, beta
    real(sp), intent(inout) :: C(:)
    integer, intent(in) :: m, n, k
    
    integer :: i, j, kk, ii, jj, kkk
    integer :: tile_m, tile_n, tile_k
    real(sp) :: sum
    
    ! Tile sizes optimized for L1 cache (32KB typical)
    tile_m = 32    ! Multiple of 16 for AVX-512, fits L1 cache
    tile_n = 64    ! Keep reasonable N tile size
    tile_k = 64    ! Reduced K tile to fit in L1 cache
    
    ! Initialize C with beta scaling
    if (beta /= 1.0) then
      if (beta == 0.0) then
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
    
    ! Tiled matrix multiplication with OpenMP parallelization
    !$omp parallel do collapse(2) private(i, j, kk, ii, jj, kkk, sum) schedule(static)
    do jj = 1, n, tile_n
      do ii = 1, m, tile_m
        ! K-loop (innermost for better cache reuse)
        do kkk = 1, k, tile_k
          ! Micro-kernel
          do j = jj, min(jj + tile_n - 1, n)
            do kk = kkk, min(kkk + tile_k - 1, k)
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
    
  end subroutine gemm_simd_avx512_packed
  
  ! Flexible strided implementation
  subroutine gemm_simd_avx512_strided(A, B, C, m, n, k, alpha, beta, lda, ldb, ldc)
    real(sp), intent(in) :: A(:), B(:), alpha, beta
    real(sp), intent(inout) :: C(:)
    integer, intent(in) :: m, n, k, lda, ldb, ldc
    
    integer :: i, j, kk
    real(sp) :: sum
    
    ! Initialize C with beta scaling
    if (beta == 0.0) then
      !$omp parallel do collapse(2)
      do j = 1, n
        do i = 1, m
          C((i-1) + (j-1)*ldc + 1) = 0.0
        end do
      end do
      !$omp end parallel do
    else if (beta /= 1.0) then
      !$omp parallel do collapse(2)
      do j = 1, n
        do i = 1, m
          C((i-1) + (j-1)*ldc + 1) = beta * C((i-1) + (j-1)*ldc + 1)
        end do
      end do
      !$omp end parallel do
    end if
    
    ! Strided GEMM - still optimized but handles arbitrary strides
    ! Using pointer arithmetic as Mini suggested
    !$omp parallel do collapse(2) private(i, j, kk, sum)
    do j = 1, n
      do i = 1, m
        sum = 0.0
        do kk = 1, k
          ! Pointer math (column-major):
          ! A(i, kk) -> A_base + (i-1) + (kk-1)*lda
          ! B(kk, j) -> B_base + (kk-1) + (j-1)*ldb
          sum = sum + A((i-1) + (kk-1)*lda + 1) * B((kk-1) + (j-1)*ldb + 1)
        end do
        ! C(i, j) -> C_base + (i-1) + (j-1)*ldc
        C((i-1) + (j-1)*ldc + 1) = C((i-1) + (j-1)*ldc + 1) + alpha * sum
      end do
    end do
    !$omp end parallel do
    
  end subroutine gemm_simd_avx512_strided

end module gemm_simd_optimized_v2