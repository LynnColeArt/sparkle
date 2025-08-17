program test_cpu_conv2d_direct
  use iso_fortran_env, only: real32, real64, int64
  use cpu_conv2d_simd, only: conv2d_cpu_simd
  implicit none
  
  ! Test parameters - same as benchmark
  integer, parameter :: N = 1, C = 64, H = 56, W = 56
  integer, parameter :: K = 256, kernel_size = 3, stride = 1, pad = 1
  integer, parameter :: H_out = 56, W_out = 56
  
  real(real32), allocatable :: input(:), weights(:), output(:)
  real(real32) :: time_ms
  integer :: i
  
  print *, "🚀 Direct CPU Conv2D SIMD Test"
  print *, "=============================="
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  
  print *, "🔥 Running 5 iterations..."
  print *, ""
  
  do i = 1, 5
    output = 0.0
    print '(A,I0,A)', "Run ", i, ":"
    time_ms = conv2d_cpu_simd(input, weights, output, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    print *, ""
  end do
  
  print *, "✅ Test complete!"
  
  deallocate(input, weights, output)
  
end program test_cpu_conv2d_direct