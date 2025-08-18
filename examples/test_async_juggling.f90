! Test async GPU executor integration with device juggling
program test_async_juggling
  use iso_fortran_env, only: real32, int64
  use sparkle_conv2d_juggling
  implicit none
  
  ! Test parameters - large enough to trigger GPU
  integer, parameter :: N = 1, C = 64, H = 56, W = 56
  integer, parameter :: K = 256, kernel_size = 3, stride = 1, pad = 1
  integer :: H_out, W_out
  
  ! Arrays
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size
  real(real32) :: time_ms
  integer :: i
  
  ! Calculate output dimensions
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  print *, "🚀 Testing Async GPU Executor Integration"
  print *, "========================================"
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "×", C, "×", H, "×", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output: ", N, "×", K, "×", H_out, "×", W_out
  print *, ""
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  
  ! Initialize with test data
  do i = 1, input_size
    input(i) = real(mod(i-1, 256), real32) / 256.0
  end do
  
  do i = 1, weight_size
    weights(i) = real(mod(i-1, 64), real32) / 64.0 - 0.5
  end do
  
  ! Initialize juggling system
  call init_juggling_system()
  
  print *, "🔥 Running convolution with device juggling..."
  
  ! Run convolution - should automatically select GPU async
  time_ms = conv2d_auto_juggling(input, weights, output, &
                                N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print '(A,F8.2,A)', "✅ Convolution completed in ", time_ms, " ms"
  
  ! Calculate GFLOPS
  block
    integer(int64) :: total_flops
    real(real32) :: gflops
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops) / (time_ms * 1e6)
    print '(A,F8.1,A)', "🎯 Performance: ", gflops, " GFLOPS"
  end block
  
  ! Test disabling async
  print *, ""
  print *, "📊 Testing with async disabled..."
  call disable_async_gpu()
  
  time_ms = conv2d_auto_juggling(input, weights, output, &
                                N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print '(A,F8.2,A)', "✅ Synchronous convolution: ", time_ms, " ms"
  
  ! Re-enable async
  print *, ""
  print *, "🚀 Re-enabling async..."
  call enable_async_gpu()
  
  time_ms = conv2d_auto_juggling(input, weights, output, &
                                N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print '(A,F8.2,A)', "✅ Async convolution: ", time_ms, " ms"
  
  ! Cleanup
  call cleanup_juggling_system()
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "✨ Test completed successfully!"
  
end program test_async_juggling