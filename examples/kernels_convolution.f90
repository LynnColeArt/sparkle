! ========================================================================
! Convolution kernels for Fortran GPU DSL
! 
! These kernels implement the im2col + GEMM approach to convolution
! They're designed to work with our adaptive parameter passing system
! ========================================================================

! Transform input tensor to column matrix for GEMM
! This is the memory-bound part of convolution
!@kernel(local_size_x=256, in=1, out=1)
pure subroutine im2col_nhwc(idx, input, col_matrix, &
                            batch_size, height, width, channels, &
                            kernel_h, kernel_w, &
                            stride_h, stride_w, &
                            pad_h, pad_w, &
                            output_h, output_w)
  use iso_fortran_env
  integer(int32), value :: idx
  real(real32), intent(in) :: input         ! Input tensor (flattened NHWC)
  real(real32), intent(out) :: col_matrix   ! Output column matrix
  
  ! All these are scalar parameters - perfect for adaptive passing!
  integer(int32), value :: batch_size
  integer(int32), value :: height, width, channels
  integer(int32), value :: kernel_h, kernel_w
  integer(int32), value :: stride_h, stride_w
  integer(int32), value :: pad_h, pad_w
  integer(int32), value :: output_h, output_w
  
  ! Local variables for computation
  integer :: n, h_out, w_out, h_in, w_in, kh, kw, c
  integer :: col_idx, input_idx
  integer :: num_output_pixels
  
  ! Each thread handles one element of the column matrix
  ! col_matrix is (C*KH*KW) x (N*H_out*W_out)
  
  num_output_pixels = output_h * output_w
  
  ! Compute which element this thread handles
  n = idx / (channels * kernel_h * kernel_w * num_output_pixels)
  col_idx = mod(idx, channels * kernel_h * kernel_w * num_output_pixels)
  
  ! Decompose column index
  c = col_idx / (kernel_h * kernel_w * num_output_pixels)
  kh = mod(col_idx / (kernel_w * num_output_pixels), kernel_h)
  kw = mod(col_idx / num_output_pixels, kernel_w)
  
  h_out = mod(col_idx / output_w, output_h)
  w_out = mod(col_idx, output_w)
  
  ! Compute input coordinates
  h_in = h_out * stride_h - pad_h + kh
  w_in = w_out * stride_w - pad_w + kw
  
  ! Check bounds and copy
  if (h_in >= 0 .and. h_in < height .and. &
      w_in >= 0 .and. w_in < width) then
    input_idx = ((n * height + h_in) * width + w_in) * channels + c
    col_matrix = input(input_idx)
  else
    col_matrix = 0.0  ! Padding
  end if
  
end subroutine im2col_nhwc

! Tiled GEMM kernel optimized for convolution
! This is the compute-bound part where we want 14 TFLOPS
!@kernel(local_size_x=16, local_size_y=16, in=2, out=1)
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
  
  ! Thread block and position
  integer :: bx, by, tx, ty
  integer :: row, col
  real(real32) :: sum
  integer :: k_idx
  
  ! For now, simple non-tiled version
  ! TODO: Add shared memory tiling for performance
  
  row = idx
  col = idy
  
  if (row < M .and. col < N) then
    sum = 0.0
    
    ! Compute dot product
    do k_idx = 0, K-1
      sum = sum + A(row * K + k_idx) * B(k_idx * N + col)
    end do
    
    ! Write result with alpha/beta scaling
    C(row * N + col) = alpha * sum + beta * C(row * N + col)
  end if
  
end subroutine gemm_tiled

! Simple convolution kernel for small filters (3x3, 5x5)
! This might run better on CPU or iGPU due to lower arithmetic intensity
!@kernel(local_size_x=256, in=2, out=1)
pure subroutine conv2d_direct(idx, input, kernel, output, &
                              batch_size, height, width, &
                              in_channels, out_channels, &
                              kernel_h, kernel_w, &
                              stride_h, stride_w)
  use iso_fortran_env
  integer(int32), value :: idx
  real(real32), intent(in) :: input    ! NCHW format
  real(real32), intent(in) :: kernel   ! OIHW format
  real(real32), intent(out) :: output  ! NCHW format
  
  integer(int32), value :: batch_size
  integer(int32), value :: height, width
  integer(int32), value :: in_channels, out_channels
  integer(int32), value :: kernel_h, kernel_w
  integer(int32), value :: stride_h, stride_w
  
  integer :: n, oc, oh, ow, ic, kh, kw
  integer :: out_h, out_w
  integer :: in_h, in_w
  real(real32) :: sum
  integer :: out_idx, in_idx, k_idx
  
  ! Output dimensions
  out_h = (height - kernel_h) / stride_h + 1
  out_w = (width - kernel_w) / stride_w + 1
  
  ! Decode output position from thread index
  n = idx / (out_channels * out_h * out_w)
  oc = mod(idx / (out_h * out_w), out_channels)
  oh = mod(idx / out_w, out_h)
  ow = mod(idx, out_w)
  
  if (n < batch_size) then
    sum = 0.0
    
    ! Convolution sum
    do ic = 0, in_channels - 1
      do kh = 0, kernel_h - 1
        do kw = 0, kernel_w - 1
          in_h = oh * stride_h + kh
          in_w = ow * stride_w + kw
          
          if (in_h < height .and. in_w < width) then
            ! Input: NCHW layout
            in_idx = ((n * in_channels + ic) * height + in_h) * width + in_w
            ! Kernel: OIHW layout  
            k_idx = ((oc * in_channels + ic) * kernel_h + kh) * kernel_w + kw
            
            sum = sum + input(in_idx) * kernel(k_idx)
          end if
        end do
      end do
    end do
    
    ! Output: NCHW layout
    out_idx = ((n * out_channels + oc) * out_h + oh) * out_w + ow
    output(out_idx) = sum
  end if
  
end subroutine conv2d_direct