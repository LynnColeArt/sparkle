program test_manual_conv
  use iso_fortran_env
  implicit none
  
  ! Manual convolution test - 1x1x3x3 input, 1x1x3x3 filter, pad=1
  real(real32) :: input(9), weights(9), output_manual, output_gemm
  real(real32) :: patch(9)
  integer :: h_out, w_out, h_in, w_in, kh, kw
  integer :: H, W, pad, stride, kernel_size
  
  H = 3
  W = 3
  kernel_size = 3
  stride = 1
  pad = 1
  
  ! Simple input: 1-9
  input = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
  
  ! Simple weights: all 0.1
  weights = 0.1
  
  print *, "=== MANUAL CONVOLUTION TEST ==="
  print *, "Input 3x3:"
  print '(3F6.1)', input(1:3)
  print '(3F6.1)', input(4:6)
  print '(3F6.1)', input(7:9)
  
  print *, ""
  print *, "Weights 3x3 (all 0.1)"
  
  ! Compute output at position (1,1) manually
  h_out = 1
  w_out = 1
  
  print *, ""
  print *, "Computing output at position (1,1):"
  print *, "Extracting 3x3 patch with padding:"
  
  patch = 0.0
  do kh = 1, kernel_size
    do kw = 1, kernel_size
      h_in = (h_out - 1) * stride + kh - pad
      w_in = (w_out - 1) * stride + kw - pad
      
      if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
        patch((kh-1)*3 + kw) = input((h_in-1)*W + w_in)
        print '(A,I0,A,I0,A,F6.1)', "  Position (", kh, ",", kw, ") -> input value ", &
          input((h_in-1)*W + w_in)
      else
        print '(A,I0,A,I0,A)', "  Position (", kh, ",", kw, ") -> padding (0.0)"
      end if
    end do
  end do
  
  print *, ""
  print *, "Extracted patch:"
  print '(3F6.1)', patch(1:3)
  print '(3F6.1)', patch(4:6)
  print '(3F6.1)', patch(7:9)
  
  ! Manual dot product
  output_manual = 0.0
  do kh = 1, 9
    output_manual = output_manual + patch(kh) * weights(kh)
  end do
  
  print *, ""
  print '(A,F8.3)', "Manual convolution result: ", output_manual
  print '(A,F8.3)', "Expected (0+0+0+0+1+2+0+4+5)*0.1 = ", 1.2
  
end program test_manual_conv