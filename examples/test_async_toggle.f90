program test_async_toggle
  use iso_fortran_env
  use sparkle_conv2d_v2
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i
  
  ! Test parameters
  N = 1
  C = 3
  H = 224
  W = 224
  K = 64
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with test data
  input = 1.0
  weights = 0.1
  output = 0.0
  
  ! Initialize conv2d module
  call conv2d_init()
  
  print *, ""
  print *, "=== Testing Async Executor Toggle ==="
  print *, ""
  
  ! Test 1: Standard GPU execution
  print *, "1. Standard GPU execution (no async requested):"
  call conv2d(input, weights, output, &
             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
             device_hint="gpu", use_async=.false.)
  
  print *, ""
  
  ! Test 2: Async requested without environment variable
  print *, "2. Async requested but environment variable not set:"
  call conv2d(input, weights, output, &
             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
             device_hint="gpu", use_async=.true.)
  
  print *, ""
  
  ! Test 3: Show message about enabling async
  print *, "3. To enable async execution, run with:"
  print *, "   SPORKLE_GPU_ASYNC=1 ./test_async_toggle"
  print *, ""
  
  ! Show statistics
  call conv2d_show_stats()
  
  ! Cleanup
  call conv2d_cleanup()
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "✅ Test completed!"
  
end program test_async_toggle