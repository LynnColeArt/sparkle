program test_v3_simple
  use iso_fortran_env, only: real32
  use sparkle_conv2d_v3
  implicit none
  
  real(real32), allocatable :: input(:,:,:,:)
  real(real32), allocatable :: weights(:,:,:,:)
  real(real32), allocatable :: output(:,:,:,:)
  
  integer :: N=1, H=32, W=32, C=3, K=16, KH=3, KW=3
  integer :: OH, OW, i
  
  ! Calculate output dimensions
  OH = H - KH + 1
  OW = W - KW + 1
  
  ! Allocate arrays
  allocate(input(N,H,W,C))
  allocate(weights(K,KH,KW,C))
  allocate(output(N,OH,OW,K))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  
  ! Initialize V3
  call sparkle_conv2d_v3_init()
  
  print *, "🧪 Testing simple V3 execution..."
  
  ! Single execution
  call sparkle_conv2d_v3_execute(input, weights, output=output)
  
  print *, "✅ Single execution completed"
  print *, "Output sum:", sum(output)
  
  ! Multiple executions
  print *, ""
  print *, "🧪 Testing multiple executions..."
  do i = 1, 5
    call sparkle_conv2d_v3_execute(input, weights, output=output)
    print '(A,I0,A,F10.2)', "  Iteration ", i, " - sum: ", sum(output)
  end do
  
  ! Cleanup
  call sparkle_conv2d_v3_cleanup()
  
  deallocate(input, weights, output)
  
end program test_v3_simple