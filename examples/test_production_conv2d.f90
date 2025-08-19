! Test production convolution interface
! This verifies that the reference implementation has been properly integrated

program test_production_conv2d
  use kinds
  use sporkle_conv2d, only: conv2d_cpu, conv2d_gpu, conv2d_auto
  implicit none
  
  ! ResNet-50 first layer parameters
  integer, parameter :: N = 1, C = 3, H = 224, W = 224
  integer, parameter :: K = 64, kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = 112, W_out = 112
  
  ! Arrays
  real(sp), allocatable :: input(:), weights(:), output_cpu(:), output_gpu(:)
  
  ! Dimensions
  integer :: input_size, weight_size, output_size
  integer :: i
  real(sp) :: max_diff
  
  print *, "🧪 Testing Production Conv2D Interface"
  print *, "====================================="
  print *, ""
  print '(A,I0,A,I0,A,I0,A,I0)', "Input shape: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0)', "Output shape: ", N, "x", K, "x", H_out, "x", W_out
  print '(A,I0,A,I0,A,I0,A,I0)', "Kernel: ", K, "x", C, "x", kernel_size, "x", kernel_size
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output_cpu(output_size))
  allocate(output_gpu(output_size))
  
  ! Initialize with simple test pattern
  print *, "📝 Initializing test data..."
  do i = 1, input_size
    input(i) = real(mod(i-1, 256), real32) / 256.0
  end do
  
  do i = 1, weight_size
    weights(i) = real(mod(i-1, 64), real32) / 64.0 - 0.5
  end do
  
  ! Test CPU implementation
  print *, "🖥️  Testing CPU implementation..."
  call conv2d_cpu(input, weights, output_cpu, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Test GPU implementation
  print *, "🎮 Testing GPU implementation..."
  call conv2d_gpu(input, weights, output_gpu, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Compare results
  print *, ""
  print *, "📊 Comparing Results:"
  max_diff = 0.0
  do i = 1, output_size
    max_diff = max(max_diff, abs(output_cpu(i) - output_gpu(i)))
  end do
  
  print '(A,E12.4)', "Maximum difference: ", max_diff
  
  if (max_diff < 1.0e-5) then
    print *, "✅ CPU and GPU results match!"
  else
    print *, "❌ CPU and GPU results differ significantly"
  end if
  
  ! Test auto-selection
  print *, ""
  print *, "🤖 Testing auto-selection..."
  call conv2d_auto(input, weights, output_gpu, &
                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "🎉 Production interface test complete!"
  
  deallocate(input, weights, output_cpu, output_gpu)
  
end program test_production_conv2d