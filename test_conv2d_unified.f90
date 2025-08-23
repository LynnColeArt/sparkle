program test_conv2d_unified
  ! Test the unified Conv2D implementation with automatic device selection
  use kinds
  use sporkle_conv2d_unified
  implicit none
  
  ! Test parameters
  integer, parameter :: N = 1          ! Batch size
  integer, parameter :: C = 3          ! Input channels
  integer, parameter :: H = 224        ! Height
  integer, parameter :: W = 224        ! Width
  integer, parameter :: K = 64         ! Output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  
  ! Arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  real(sp) :: time_ms
  integer :: H_out, W_out
  integer :: i
  
  print *, "=== Testing Unified Conv2D Implementation ==="
  print *, ""
  
  ! Calculate output dimensions
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "Input shape:  ", N, "x", C, "x", H, "x", W
  print *, "Weight shape: ", K, "x", C, "x", kernel_size, "x", kernel_size
  print *, "Output shape: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Allocate arrays
  allocate(input(N*C*H*W))
  allocate(weights(K*C*kernel_size*kernel_size))
  allocate(output(N*K*H_out*W_out))
  
  ! Initialize with simple pattern
  do i = 1, size(input)
    input(i) = real(mod(i, 100), sp) * 0.01_sp
  end do
  
  do i = 1, size(weights)
    weights(i) = real(mod(i, 10), sp) * 0.1_sp
  end do
  
  output = 0.0_sp
  
  ! Test 1: Auto device selection
  print *, "Test 1: Automatic device selection"
  time_ms = sporkle_conv2d_unified(input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad)
  
  if (time_ms > 0.0_sp) then
    print '(A,F8.2,A)', "  Time: ", time_ms, " ms"
    
    ! Check first few outputs
    print *, "  First 5 outputs:", output(1:min(5, size(output)))
  else
    print *, "  ❌ Execution failed"
  end if
  print *, ""
  
  ! Test 2: Force CPU
  print *, "Test 2: Force CPU execution"
  output = 0.0_sp
  time_ms = sporkle_conv2d_unified(input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad, &
                                  device_type="cpu")
  
  if (time_ms > 0.0_sp) then
    print '(A,F8.2,A)', "  Time: ", time_ms, " ms"
    print *, "  First 5 outputs:", output(1:min(5, size(output)))
  else
    print *, "  ❌ Execution failed"
  end if
  print *, ""
  
  ! Test 3: Try GPU
  print *, "Test 3: Try GPU execution (PM4)"
  output = 0.0_sp
  time_ms = sporkle_conv2d_unified(input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad, &
                                  device_type="gpu")
  
  if (time_ms > 0.0_sp) then
    print '(A,F8.2,A)', "  Time: ", time_ms, " ms"
    print *, "  First 5 outputs:", output(1:min(5, size(output)))
  else
    print *, "  ❌ GPU execution failed (expected if waves not launching)"
  end if
  
  ! Cleanup
  deallocate(input, weights, output)
  
  print *, ""
  print *, "✅ Test complete"
  
end program test_conv2d_unified