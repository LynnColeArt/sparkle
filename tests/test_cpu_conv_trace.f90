program test_cpu_conv_trace
  implicit none
  
  ! ResNet-50 first layer params
  integer, parameter :: N = 1, H = 224, W = 224, C = 3, K = 64
  integer, parameter :: kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = (H + 2*pad - kernel_size) / stride + 1
  integer, parameter :: W_out = (W + 2*pad - kernel_size) / stride + 1
  
  integer :: h_in, w_in
  integer :: h_out_idx, w_out_idx, kh_idx, kw_idx
  
  print *, "ResNet-50 first layer convolution:"
  print *, "H_out = ", H_out, " W_out = ", W_out
  print *, ""
  
  ! Trace first output position
  h_out_idx = 1
  w_out_idx = 1
  
  print *, "For output position (", h_out_idx, ",", w_out_idx, "):"
  print *, "Kernel positions that contribute:"
  
  do kh_idx = 1, kernel_size
    do kw_idx = 1, kernel_size
      h_in = (h_out_idx - 1) * stride + (kh_idx - 1) - pad + 1
      w_in = (w_out_idx - 1) * stride + (kw_idx - 1) - pad + 1
      
      if (h_in > 0 .and. h_in <= H .and. w_in > 0 .and. w_in <= W) then
        print '(A,I1,A,I1,A,I3,A,I3,A)', &
          "  kh=", kh_idx, " kw=", kw_idx, " -> (", h_in, ",", w_in, ") - VALID"
      else
        print '(A,I1,A,I1,A,I3,A,I3,A)', &
          "  kh=", kh_idx, " kw=", kw_idx, " -> (", h_in, ",", w_in, ") - OUT OF BOUNDS"
      end if
    end do
  end do
  
  print *, ""
  print *, "For last output position (", H_out, ",", W_out, "):"
  h_out_idx = H_out
  w_out_idx = W_out
  
  do kh_idx = 1, min(3, kernel_size)
    do kw_idx = 1, min(3, kernel_size)
      h_in = (h_out_idx - 1) * stride + (kh_idx - 1) - pad + 1
      w_in = (w_out_idx - 1) * stride + (kw_idx - 1) - pad + 1
      
      if (h_in > 0 .and. h_in <= H .and. w_in > 0 .and. w_in <= W) then
        print '(A,I1,A,I1,A,I3,A,I3,A)', &
          "  kh=", kh_idx, " kw=", kw_idx, " -> (", h_in, ",", w_in, ") - VALID"
      else
        print '(A,I1,A,I1,A,I3,A,I3,A)', &
          "  kh=", kh_idx, " kw=", kw_idx, " -> (", h_in, ",", w_in, ") - OUT OF BOUNDS"
      end if
    end do
  end do
  
end program test_cpu_conv_trace