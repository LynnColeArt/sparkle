! Test correctness of CPU convolution implementation
program test_conv2d_correctness
  use iso_fortran_env, only: real32
  use sparkle_conv2d, only: conv2d_cpu, conv2d_gpu, conv2d_select_implementation
  implicit none
  
  ! Small test case for easy verification
  integer, parameter :: N = 1, C = 1, H = 3, W = 3
  integer, parameter :: K = 1, kernel_size = 3, stride = 1, pad = 1
  integer, parameter :: H_out = 3, W_out = 3
  
  real(real32) :: input(N*C*H*W)
  real(real32) :: weights(K*C*kernel_size*kernel_size)
  real(real32) :: output_cpu(N*K*H_out*W_out)
  real(real32) :: output_gpu(N*K*H_out*W_out)
  real(real32) :: expected(N*K*H_out*W_out)
  
  integer :: i
  real(real32) :: max_diff
  
  print *, "Testing convolution correctness with simple 3x3 input"
  print *, ""
  
  ! Initialize simple test pattern
  ! Input (3x3):
  ! 1 2 3
  ! 4 5 6  
  ! 7 8 9
  input = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
  
  ! Weights (3x3) - simple averaging kernel
  weights = [1.0/9.0, 1.0/9.0, 1.0/9.0, &
             1.0/9.0, 1.0/9.0, 1.0/9.0, &
             1.0/9.0, 1.0/9.0, 1.0/9.0]
  
  ! Expected output with pad=1 (edges padded with 0):
  ! Top-left: (0+0+0+0+1+2+0+4+5)/9 = 12/9 = 1.333...
  ! Top-center: (0+0+0+1+2+3+4+5+6)/9 = 21/9 = 2.333...
  ! etc.
  expected = [12.0/9.0, 21.0/9.0, 16.0/9.0, &
              27.0/9.0, 45.0/9.0, 33.0/9.0, &
              24.0/9.0, 39.0/9.0, 28.0/9.0]
  
  print *, "Input (3x3):"
  print '(3F6.1)', input(1:3)
  print '(3F6.1)', input(4:6)
  print '(3F6.1)', input(7:9)
  print *, ""
  
  print *, "Weights (3x3 averaging kernel):"
  print '(3F8.4)', weights(1:3)
  print '(3F8.4)', weights(4:6)
  print '(3F8.4)', weights(7:9)
  print *, ""
  
  ! Test CPU implementation
  call conv2d_select_implementation("cpu", "naive")  ! Use naive for correctness check
  call conv2d_cpu(input, weights, output_cpu, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "CPU output (naive):"
  print '(3F8.4)', output_cpu(1:3)
  print '(3F8.4)', output_cpu(4:6)
  print '(3F8.4)', output_cpu(7:9)
  print *, ""
  
  ! Compare with expected
  max_diff = 0.0
  do i = 1, size(expected)
    max_diff = max(max_diff, abs(output_cpu(i) - expected(i)))
  end do
  print '(A,E12.4)', "Max diff from expected (naive): ", max_diff
  
  ! Test optimized CPU implementation  
  call conv2d_select_implementation("cpu", "reference")
  call conv2d_cpu(input, weights, output_cpu, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "CPU output (optimized):"
  print '(3F8.4)', output_cpu(1:3)
  print '(3F8.4)', output_cpu(4:6)
  print '(3F8.4)', output_cpu(7:9)
  
  ! Compare with expected
  max_diff = 0.0
  do i = 1, size(expected)
    max_diff = max(max_diff, abs(output_cpu(i) - expected(i)))
  end do
  print '(A,E12.4)', "Max diff from expected (optimized): ", max_diff
  
  ! Test GPU implementation
  print *, ""
  call conv2d_gpu(input, weights, output_gpu, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "GPU output:"
  print '(3F8.4)', output_gpu(1:3)
  print '(3F8.4)', output_gpu(4:6)
  print '(3F8.4)', output_gpu(7:9)
  
  ! Compare with expected
  max_diff = 0.0
  do i = 1, size(expected)
    max_diff = max(max_diff, abs(output_gpu(i) - expected(i)))
  end do
  print '(A,E12.4)', "Max diff from expected (GPU): ", max_diff
  
end program test_conv2d_correctness