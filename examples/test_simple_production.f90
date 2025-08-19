! Simple test of production interface without complex dependencies
program test_simple_production
  use kinds
  implicit none
  
  ! Simple test data
  integer, parameter :: N = 1, C = 2, H = 4, W = 4
  integer, parameter :: K = 2, kernel_size = 3, stride = 1, pad = 1
  integer, parameter :: H_out = 4, W_out = 4
  
  real(sp), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size, i
  
  print *, "🧪 Simple Production Interface Test"
  print *, "=================================="
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  print '(A,I0)', "Input size: ", input_size
  print '(A,I0)', "Weight size: ", weight_size  
  print '(A,I0)', "Output size: ", output_size
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  
  ! Initialize with simple pattern
  do i = 1, input_size
    input(i) = real(i, real32)
  end do
  
  do i = 1, weight_size
    weights(i) = 1.0
  end do
  
  output = 0.0
  
  print *, ""
  print *, "✅ Test setup complete!"
  print *, "Ready for production interface integration"
  
  ! For now, just verify our setup
  print '(A,F6.2)', "First input value: ", input(1)
  print '(A,F6.2)', "First weight value: ", weights(1)
  print '(A,F6.2)', "Output sum before: ", sum(output)
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "🎉 Simple test complete!"
  
end program test_simple_production