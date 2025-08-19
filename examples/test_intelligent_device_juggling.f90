! Test intelligent device juggling system
! Demonstrates smart frameworks for smart systems

program test_intelligent_device_juggling
  use kinds
  use intelligent_device_juggling, only: juggling_system, discover_and_profile_devices, &
                                        execute_intelligent_conv2d
  implicit none
  
  ! Test different workload sizes
  integer, parameter :: num_tests = 4
  integer, parameter :: test_sizes(num_tests, 2) = reshape([&
    64, 32,   &  ! Small: 64x64x3 -> 32x32x16 
    224, 112, &  ! Medium: 224x224x3 -> 112x112x64 (ResNet-50)
    512, 256, &  ! Large: 512x512x3 -> 256x256x128
    1024, 512 &  ! Huge: 1024x1024x3 -> 512x512x256
  ], [num_tests, 2])
  
  ! Arrays for largest test case
  real(sp), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size
  integer :: i, test_idx
  
  ! Test parameters
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(sp) :: execution_time
  integer(i64) :: total_flops
  real(sp) :: achieved_gflops
  
  print *, "🧠 Testing Intelligent Device Juggling System"
  print *, "============================================="
  print *, ""
  print *, "The breakthrough insight: Smart systems require smart frameworks."
  print *, "This test demonstrates how intelligent device selection and workload"
  print *, "distribution can improve overall performance."
  print *, ""
  
  ! Initialize the juggling system
  call discover_and_profile_devices(juggling_system)
  
  print *, ""
  print *, "🎯 Testing Workload Distribution Intelligence"
  print *, "============================================="
  
  ! Test different workload sizes to see intelligent decisions
  do test_idx = 1, num_tests
    ! Set up test parameters
    N = 1
    C = 3
    H = test_sizes(test_idx, 1)
    W = test_sizes(test_idx, 1) 
    select case(test_idx)
    case(1)
      K = 16
    case(2)
      K = 64
    case(3)
      K = 128
    case(4)
      K = 256
    case default
      K = 64
    end select
    kernel_size = 7
    stride = 2
    pad = 3
    H_out = test_sizes(test_idx, 2)
    W_out = test_sizes(test_idx, 2)
    
    ! Calculate sizes
    input_size = N * C * H * W
    weight_size = K * C * kernel_size * kernel_size
    output_size = N * K * H_out * W_out
    
    ! Allocate arrays
    if (allocated(input)) deallocate(input)
    if (allocated(weights)) deallocate(weights)
    if (allocated(output)) deallocate(output)
    
    allocate(input(input_size))
    allocate(weights(weight_size))
    allocate(output(output_size))
    
    ! Initialize with test pattern
    do i = 1, input_size
      input(i) = real(mod(i-1, 256), real32) / 256.0
    end do
    
    do i = 1, weight_size
      weights(i) = real(mod(i-1, 64), real32) / 64.0 - 0.5
    end do
    
    ! Display test case
    print *, ""
    select case(test_idx)
    case(1)
      print *, "📋 Test 1: Small Workload (should choose optimal single device)"
    case(2) 
      print *, "📋 Test 2: Medium Workload (ResNet-50 layer)"
    case(3)
      print *, "📋 Test 3: Large Workload (may benefit from hybrid execution)"
    case(4)
      print *, "📋 Test 4: Huge Workload (should definitely use hybrid)"
    end select
    
    print '(A,I0,A,I0,A,I0,A,I0)', "   Input: ", N, "x", C, "x", H, "x", W
    print '(A,I0,A,I0,A,I0,A,I0)', "   Output: ", N, "x", K, "x", H_out, "x", W_out
    print '(A,I0,A,I0,A,I0,A,I0)', "   Kernel: ", K, "x", C, "x", kernel_size, "x", kernel_size
    
    ! Calculate workload intensity
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    print '(A,F0.2,A)', "   Workload: ", real(total_flops, real32) / 1.0e9, " GFLOPS"
    
    ! Execute with intelligent juggling
    execution_time = execute_intelligent_conv2d(input, weights, output, &
                                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Performance summary
    if (execution_time > 0.0) then
      achieved_gflops = real(total_flops, real32) / (execution_time * 1.0e6)
      print '(A,F0.2,A,F0.1,A)', "   Result: ", execution_time, " ms (", achieved_gflops, " GFLOPS)"
    else
      print *, "   Result: Execution failed"
    end if
    
    print *, "   " // repeat("-", 50)
  end do
  
  print *, ""
  print *, "🎉 Intelligent Device Juggling Test Complete!"
  print *, ""
  print *, "📈 Learning Summary:"
  print '(A,I0)', "   Total runs: ", juggling_system%total_runs  
  print '(A,F0.1,A)', "   Best combined performance: ", juggling_system%best_combined_gflops, " GFLOPS"
  print '(A,F0.1,A)', "   CPU performance model: ", juggling_system%cpu_profile%conv2d_gflops, " GFLOPS"
  print '(A,F0.1,A)', "   GPU performance model: ", juggling_system%gpu_profile%conv2d_gflops, " GFLOPS"
  
  print *, ""
  print *, "🚀 Key Insights:"
  print *, "   • Smart frameworks make smart decisions about device usage"
  print *, "   • Workload-aware scheduling can improve overall efficiency"  
  print *, "   • Performance models adapt and improve over time"
  print *, "   • Different workload sizes benefit from different strategies"
  
  ! Cleanup
  if (allocated(input)) deallocate(input)
  if (allocated(weights)) deallocate(weights)
  if (allocated(output)) deallocate(output)
  
end program test_intelligent_device_juggling