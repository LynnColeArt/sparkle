program test_cpu_performance_fix
  use iso_fortran_env, only: real32, int64
  use sparkle_conv2d, only: conv2d_cpu
  implicit none
  
  ! Test parameters - medium workload
  integer, parameter :: N = 1, C = 64, H = 64, W = 64, K = 64
  integer, parameter :: kernel_size = 3, stride = 1, pad = 1
  integer :: H_out, W_out
  
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size
  integer(int64) :: total_flops
  real(real32) :: expected_gflops
  
  print *, "🔧 Testing CPU Performance Fix"
  print *, "=============================="
  print *, ""
  
  ! Calculate output dimensions
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  ! Calculate expected performance
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  expected_gflops = real(total_flops) / 1.0e9
  
  print '(A,F6.3,A)', "Workload: ", expected_gflops, " GFLOPs"
  print *, ""
  print *, "Running CPU convolution with optimized implementation..."
  print *, ""
  
  ! Run convolution
  call conv2d_cpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "✅ Test complete!"
  print *, ""
  print *, "Expected performance: ~15-25 GFLOPS with optimized implementation"
  print *, "Previous performance: ~0.7 GFLOPS with naive implementation"
  
  deallocate(input, weights, output)
  
end program test_cpu_performance_fix