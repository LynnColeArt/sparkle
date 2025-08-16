! Kernels demonstrating adaptive parameter passing

!@kernel(local_size_x=256, in=2, out=1)
pure subroutine scaled_add(i, a, b, c, scale)
  use iso_fortran_env
  integer(int32), value :: i
  real(real32), intent(in) :: a
  real(real32), intent(in) :: b
  real(real32), intent(out) :: c
  integer(int32), value :: scale
  
  c = a + real(scale, real32) * b
end subroutine scaled_add

!@kernel(local_size_x=64, in=1, out=1)
pure subroutine param_test(i, data, kernel_h, kernel_w)
  use iso_fortran_env
  integer(int32), value :: i
  real(real32), intent(inout) :: data
  integer(int32), value :: kernel_h
  integer(int32), value :: kernel_w
  
  ! Simple test - store kernel dimensions as float
  data = real(kernel_h * kernel_w, real32)
end subroutine param_test

! Simplified im2col for testing parameter passing
!@kernel(local_size_x=256, in=1, out=1)
pure subroutine im2col_simple(idx, input, col, &
                              height, width, channels, &
                              kernel_h, kernel_w, &
                              out_height, out_width)
  use iso_fortran_env
  integer(int32), value :: idx
  real(real32), intent(in) :: input
  real(real32), intent(out) :: col
  integer(int32), value :: height, width, channels
  integer(int32), value :: kernel_h, kernel_w
  integer(int32), value :: out_height, out_width
  
  integer :: out_x, out_y, c, kh, kw
  integer :: in_y, in_x, col_idx
  
  ! Decode output position
  out_x = mod(idx, out_width)
  out_y = idx / out_width
  
  if (out_y < out_height) then
    ! Simplified - just handle first channel, first kernel position
    c = 0
    kh = 0
    kw = 0
    
    in_y = out_y + kh
    in_x = out_x + kw
    
    if (in_y < height .and. in_x < width) then
      col[idx] = input[in_y * width + in_x]
    else
      col[idx] = 0.0
    end if
  end if
end subroutine im2col_simple

! Matrix multiply kernel with tiling parameters
!@kernel(local_size_x=16, local_size_y=16, in=2, out=1)
pure subroutine gemm_parameterized(gid_x, gid_y, A, B, C, M, N, K, tile_size)
  use iso_fortran_env
  integer(int32), value :: gid_x, gid_y
  real(real32), intent(in) :: A, B
  real(real32), intent(out) :: C
  integer(int32), value :: M, N, K, tile_size
  
  integer :: row, col, k
  real(real32) :: sum
  
  row = gid_y
  col = gid_x
  
  if (row < M .and. col < N) then
    sum = 0.0
    ! Simple dot product (non-tiled for parameter testing)
    do k = 0, K-1
      sum = sum + A[row * K + k] * B[k * N + col]
    end do
    C[row * N + col] = sum
  end if
end subroutine gemm_parameterized