program test_cpu_conv_debug
  implicit none
  
  ! Small test case: 1x1x3x3 input, 1x1x3x3 kernel, no padding, stride 1
  ! Expected output: 1x1x1x1
  integer, parameter :: N = 1, H = 3, W = 3, C = 1, K = 1
  integer, parameter :: kernel_size = 3, stride = 1, pad = 0
  integer, parameter :: H_out = (H + 2*pad - kernel_size) / stride + 1
  integer, parameter :: W_out = (W + 2*pad - kernel_size) / stride + 1
  
  real :: input(N*C*H*W)
  real :: weights(K*C*kernel_size*kernel_size)
  real :: output(N*K*H_out*W_out)
  real :: expected
  
  integer :: i, n_idx, k_idx, h_out_idx, w_out_idx, c_idx, kh_idx, kw_idx, h_in, w_in
  integer :: in_idx, weight_idx, out_idx
  real :: sum
  
  print *, "Small convolution test:"
  print *, "Input shape: ", N, "x", C, "x", H, "x", W
  print *, "Weight shape: ", K, "x", C, "x", kernel_size, "x", kernel_size
  print *, "Output shape: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Initialize with simple values
  ! Input: 1 2 3
  !        4 5 6
  !        7 8 9
  do i = 1, 9
    input(i) = real(i)
  end do
  
  ! Weights: all 1s
  weights = 1.0
  
  print *, "Input:"
  do i = 0, 2
    print '(3F6.1)', input(i*3+1:i*3+3)
  end do
  print *, ""
  
  print *, "Weights: all 1.0"
  print *, ""
  
  ! Expected: sum of all input values = 1+2+3+4+5+6+7+8+9 = 45
  expected = 45.0
  
  ! Perform convolution
  output = 0.0
  
  do n_idx = 1, N
    do k_idx = 1, K
      do h_out_idx = 1, H_out
        do w_out_idx = 1, W_out
          sum = 0.0
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                ! Calculate input position
                h_in = (h_out_idx - 1) * stride + (kh_idx - 1) - pad + 1
                w_in = (w_out_idx - 1) * stride + (kw_idx - 1) - pad + 1
                
                print '(A,I1,A,I1,A,I2,A,I2)', &
                  "  kh=", kh_idx, " kw=", kw_idx, " -> h_in=", h_in, " w_in=", w_in
                
                if (h_in > 0 .and. h_in <= H .and. w_in > 0 .and. w_in <= W) then
                  ! Calculate indices
                  in_idx = (n_idx-1)*C*H*W + (c_idx-1)*H*W + (h_in-1)*W + w_in
                  weight_idx = (k_idx-1)*C*kernel_size*kernel_size + &
                               (c_idx-1)*kernel_size*kernel_size + &
                               (kh_idx-1)*kernel_size + kw_idx
                  
                  print '(A,I2,A,F5.1,A,I2,A,F5.1,A,F5.1)', &
                    "    in_idx=", in_idx, " val=", input(in_idx), &
                    " weight_idx=", weight_idx, " val=", weights(weight_idx), &
                    " prod=", input(in_idx) * weights(weight_idx)
                  
                  sum = sum + input(in_idx) * weights(weight_idx)
                else
                  print *, "    Out of bounds"
                end if
              end do
            end do
          end do
          
          out_idx = (n_idx-1)*K*H_out*W_out + (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
          output(out_idx) = sum
          
          print *, ""
          print *, "Output[", out_idx, "] = ", sum
        end do
      end do
    end do
  end do
  
  print *, ""
  print *, "Expected output: ", expected
  print *, "Actual output: ", output(1)
  print *, "Match: ", abs(output(1) - expected) < 1e-6
  
end program test_cpu_conv_debug