! Simple test of automatic device selection
program test_auto_selection_simple
  use iso_fortran_env, only: real32, int64
  use sparkle_conv2d_auto_selector
  implicit none
  
  ! Arrays
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: i
  real(real32) :: time_ms
  integer(int64) :: total_flops
  real(real32) :: gflops
  
  ! Test 1: Tiny workload (should use CPU)
  integer, parameter :: N1 = 1, C1 = 3, H1 = 32, W1 = 32, K1 = 64
  integer, parameter :: kernel_size1 = 3, stride1 = 1, pad1 = 1
  integer :: H_out1, W_out1
  
  ! Test 2: Large workload (should use GPU)
  integer, parameter :: N2 = 4, C2 = 128, H2 = 56, W2 = 56, K2 = 256
  integer, parameter :: kernel_size2 = 3, stride2 = 1, pad2 = 1
  integer :: H_out2, W_out2
  
  print *, "🤖 Simple Auto Device Selection Test"
  print *, "===================================="
  print *, ""
  
  ! Initialize
  call init_auto_selector()
  
  ! Test 1: Tiny workload
  print *, "Test 1: Tiny workload (1×3×32×32)"
  H_out1 = (H1 + 2*pad1 - kernel_size1) / stride1 + 1
  W_out1 = (W1 + 2*pad1 - kernel_size1) / stride1 + 1
  
  allocate(input(N1*C1*H1*W1))
  allocate(weights(K1*C1*kernel_size1*kernel_size1))
  allocate(output(N1*K1*H_out1*W_out1))
  
  call random_number(input)
  call random_number(weights)
  
  time_ms = conv2d_auto_select(input, weights, output, &
                               N1, C1, H1, W1, K1, kernel_size1, stride1, pad1, H_out1, W_out1)
  
  total_flops = int(N1, int64) * int(K1, int64) * int(H_out1, int64) * int(W_out1, int64) * &
                int(C1, int64) * int(kernel_size1, int64) * int(kernel_size1, int64) * 2_int64
  gflops = real(total_flops) / (time_ms * 1e6)
  
  print '(A,F8.2,A,F8.1,A)', "Result: ", time_ms, " ms (", gflops, " GFLOPS)"
  print *, ""
  
  deallocate(input, weights, output)
  
  ! Test 2: Large workload
  print *, "Test 2: Large workload (4×128×56×56)"
  H_out2 = (H2 + 2*pad2 - kernel_size2) / stride2 + 1
  W_out2 = (W2 + 2*pad2 - kernel_size2) / stride2 + 1
  
  allocate(input(N2*C2*H2*W2))
  allocate(weights(K2*C2*kernel_size2*kernel_size2))
  allocate(output(N2*K2*H_out2*W_out2))
  
  call random_number(input)
  call random_number(weights)
  
  time_ms = conv2d_auto_select(input, weights, output, &
                               N2, C2, H2, W2, K2, kernel_size2, stride2, pad2, H_out2, W_out2)
  
  total_flops = int(N2, int64) * int(K2, int64) * int(H_out2, int64) * int(W_out2, int64) * &
                int(C2, int64) * int(kernel_size2, int64) * int(kernel_size2, int64) * 2_int64
  gflops = real(total_flops) / (time_ms * 1e6)
  
  print '(A,F8.2,A,F8.1,A)', "Result: ", time_ms, " ms (", gflops, " GFLOPS)"
  print *, ""
  
  ! Show stats
  call get_selector_stats()
  
  ! Cleanup
  call cleanup_auto_selector()
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "✅ Test completed!"
  
end program test_auto_selection_simple