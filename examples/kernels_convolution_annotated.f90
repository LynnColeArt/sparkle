! Convolution kernels with proper annotations
!@kernel(local_size_x=256, in=2, out=1)
pure subroutine gemm_tiled(idx, idy, A, B, C, &
                           M, N, K, alpha, beta, &
                           tile_m, tile_n, tile_k)
  use iso_fortran_env
  integer(int32), value :: idx, idy
  real(real32), intent(in) :: A      ! Weight matrix (M x K)
  real(real32), intent(in) :: B      ! Col matrix (K x N)
  real(real32), intent(inout) :: C   ! Output (M x N)
  
  ! Matrix dimensions and parameters
  integer(int32), value :: M, N, K
  real(real32), value :: alpha, beta
  integer(int32), value :: tile_m, tile_n, tile_k
  
  ! Simple GEMM computation
  integer :: row, col, k_idx
  real(real32) :: sum
  
  row = idx / N
  col = mod(idx, N)
  
  if (row < M .and. col < N) then
    sum = 0.0
    do k_idx = 0, K-1
      sum = sum + A * B  ! Simplified - would need proper indexing
    end do
    C = alpha * sum + beta * C
  end if
end subroutine gemm_tiled