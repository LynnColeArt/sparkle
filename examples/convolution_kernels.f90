module convolution_kernels
  use iso_fortran_env
  implicit none

contains

  !@kernel local_size=[64, 1, 1]
  subroutine conv2d_direct(idx, input_data, weights, output_data, &
                          N, H, W, C, K, kernel_size, stride, pad, H_out, W_out) 
    integer(int32), intent(in), value :: idx
    real(real32), intent(in) :: input_data(:), weights(:)
    real(real32), intent(out) :: output_data(:)
    integer(int32), intent(in), value :: N, H, W, C, K
    integer(int32), intent(in), value :: kernel_size, stride, pad, H_out, W_out
    
    integer :: n, k, h_out, w_out, c, kh, kw, h_in, w_in
    integer :: out_idx, in_idx, weight_idx
    real(real32) :: sum
    
    ! Check bounds
    if (idx >= N * K * H_out * W_out) return
    
    ! Decode output position
    n = idx / (K * H_out * W_out)
    k = mod(idx / (H_out * W_out), K)
    h_out = mod(idx / W_out, H_out)
    w_out = mod(idx, W_out)
    
    sum = 0.0
    
    ! Convolution
    do c = 0, C - 1
      do kh = 0, kernel_size - 1
        do kw = 0, kernel_size - 1
          h_in = h_out * stride + kh - pad
          w_in = w_out * stride + kw - pad
          
          if (h_in >= 0 .and. h_in < H .and. w_in >= 0 .and. w_in < W) then
            in_idx = ((n * C + c) * H + h_in) * W + w_in + 1
            weight_idx = ((k * C + c) * kernel_size + kh) * kernel_size + kw + 1
            sum = sum + input_data(in_idx) * weights(weight_idx)
          end if
        end do
      end do
    end do
    
    out_idx = idx + 1
    output_data(out_idx) = sum
    
  end subroutine conv2d_direct

end module convolution_kernels