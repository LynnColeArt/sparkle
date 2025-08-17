program test_debug_fused
  use iso_fortran_env
  use cpu_conv2d_fused
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  real(real32), allocatable :: im2col_debug(:,:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i, j, ky, kx, out_y, out_x, out_idx, c_idx
  integer :: in_y, in_x, input_offset, weight_offset
  integer :: input_rows
  real(real32) :: max_diff
  
  ! Tiny test case for debugging
  N = 1
  C = 2
  H = 4
  W = 4
  K = 2
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "=== DEBUG FUSED CONVOLUTION ==="
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with simple pattern
  do i = 1, size(input)
    input(i) = real(i, real32)
  end do
  
  do i = 1, size(weights)
    weights(i) = 1.0 / real(size(weights), real32)
  end do
  
  ! Run unfused version
  print *, "Running unfused version..."
  output1 = 0.0
  max_diff = fused_conv2d_cpu(input, weights, output1, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Manual im2col to understand the pattern
  input_rows = C * kernel_size * kernel_size
  allocate(im2col_debug(input_rows, H_out * W_out))
  
  print *, ""
  print *, "Manual im2col extraction for first output location:"
  
  ! Extract first patch manually
  out_idx = 1
  out_y = 1
  out_x = 1
  
  do c_idx = 1, C
    do ky = 1, kernel_size
      do kx = 1, kernel_size
        in_y = (out_y - 1) * stride + ky - pad
        in_x = (out_x - 1) * stride + kx - pad
        
        weight_offset = ((c_idx-1)*kernel_size + (ky-1))*kernel_size + kx
        
        if (in_y >= 1 .and. in_y <= H .and. in_x >= 1 .and. in_x <= W) then
          input_offset = ((N-1)*C + (c_idx-1))*H*W + (in_y-1)*W + in_x
          print '(A,I0,A,I0,A,I0,A,I0,A,F5.1)', &
            "  c=", c_idx, " ky=", ky, " kx=", kx, " weight_offset=", weight_offset, " value=", input(input_offset)
        else
          print '(A,I0,A,I0,A,I0,A,I0,A)', &
            "  c=", c_idx, " ky=", ky, " kx=", kx, " weight_offset=", weight_offset, " value=0.0 (pad)"
        end if
      end do
    end do
  end do
  
  ! Run fused version
  print *, ""
  print *, "Running fused version..."
  output2 = 0.0
  max_diff = conv2d_fused_hot_cache(input, weights, output2, &
                                    N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Compare outputs
  print *, ""
  print *, "Output comparison (first few values):"
  do i = 1, min(10, size(output1))
    print '(A,I3,A,F8.3,A,F8.3,A,F8.3)', &
      "output[", i, "] unfused=", output1(i), " fused=", output2(i), " diff=", abs(output1(i) - output2(i))
  end do
  
  max_diff = maxval(abs(output1 - output2))
  print *, ""
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-5) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ!"
  end if
  
  deallocate(input, weights, output1, output2, im2col_debug)
  
end program test_debug_fused