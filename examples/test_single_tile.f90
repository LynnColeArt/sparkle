program test_single_tile
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff
  integer :: i
  
  print *, "=== SINGLE TILE TEST ==="
  print *, ""
  
  ! Size that fits in single tile (64 output locations)
  N = 1
  C = 2
  H = 8
  W = 8
  K = 2
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0)', "Output size: ", H_out, "x", W_out
  print '(A,I0)', "Total output locations: ", H_out * W_out
  print '(A)', "This should fit in one 64-element tile"
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize
  call random_number(input)
  call random_number(weights)
  
  ! Run both
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Check
  max_diff = maxval(abs(output1 - output2))
  
  print *, ""
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-4) then
    print *, "✅ Single tile works correctly!"
  else
    print *, "❌ Even single tile has issues"
    
    ! Show where they differ
    print *, ""
    print *, "First 10 values comparison:"
    print *, "Unfused:", output1(1:10)
    print *, "Fused:  ", output2(1:10)
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_single_tile