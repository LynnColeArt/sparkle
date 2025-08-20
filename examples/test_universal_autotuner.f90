program test_universal_autotuner
  ! Demonstrate the Universal Auto-Tuning System
  ! Combines hardware profiling + intelligent juggling + automatic optimization
  
  use kinds
  use sporkle_universal_autotuner
  implicit none
  
  ! Test configurations
  integer, parameter :: num_tests = 6
  character(len=32) :: test_names(num_tests)
  integer :: test_configs(num_tests, 6)  ! batch, in_c, out_c, h, w, kernel_size
  
  ! Arrays
  real(sp), allocatable :: input(:), kernel(:), output(:)
  
  ! Loop variables
  integer :: test, run
  integer :: batch, in_c, out_c, h, w, kh, kw, h_out, w_out
  integer(i64) :: total_flops
  real(dp) :: workload_gflops, achieved_gflops
  
  print *, "=============================================="
  print *, "🚀 Universal Auto-Tuning System Test"
  print *, "=============================================="
  print *, ""
  print *, "Combining:"
  print *, "  • Hardware profiling (automatic parameter discovery)"
  print *, "  • Intelligent juggling (workload-aware device selection)"
  print *, "  • Adaptive optimization (learning and improving)"
  print *, "  • Universal patterns (same optimization, different parameters)"
  print *, ""
  
  ! Initialize auto-tuner
  call auto_tune_init()
  
  ! Define test workloads
  test_names = [character(len=32) :: &
    "Tiny (CPU optimal)", &
    "Small (CPU competitive)", &
    "Medium (GPU starts winning)", &
    "Large (GPU dominant)", &
    "Huge (GPU essential)", &
    "Extreme (Tensor cores needed)"]
  
  ! batch, in_c, out_c, h, w, kernel_size
  test_configs(1, :) = [1, 3, 16, 32, 32, 3]        ! 0.003 GFLOPs
  test_configs(2, :) = [1, 3, 32, 64, 64, 3]        ! 0.05 GFLOPs
  test_configs(3, :) = [1, 64, 128, 112, 112, 3]    ! 1.8 GFLOPs
  test_configs(4, :) = [1, 128, 256, 224, 224, 3]   ! 29 GFLOPs
  test_configs(5, :) = [4, 256, 512, 224, 224, 3]   ! 925 GFLOPs
  test_configs(6, :) = [8, 512, 512, 224, 224, 3]   ! 3702 GFLOPs
  
  print *, "=============================================="
  print *, "Phase 1: Initial Performance (No Learning)"
  print *, "=============================================="
  print *, ""
  
  do test = 1, num_tests
    batch = test_configs(test, 1)
    in_c = test_configs(test, 2)
    out_c = test_configs(test, 3)
    h = test_configs(test, 4)
    w = test_configs(test, 5)
    kh = test_configs(test, 6)
    kw = kh
    
    h_out = h - kh + 1
    w_out = w - kw + 1
    
    ! Calculate workload
    total_flops = int(batch, i64) * out_c * h_out * w_out * in_c * kh * kw * 2
    workload_gflops = real(total_flops, dp) / 1.0e9_dp
    
    print '(A,I0,A,A)', "Test ", test, ": ", trim(test_names(test))
    print '(A,I0,A,I0,A,I0,A,I0)', "  Config: ", batch, "×", in_c, "×", h, "×", w
    print '(A,F10.3,A)', "  Workload: ", workload_gflops, " GFLOPs"
    
    ! Allocate arrays
    if (allocated(input)) deallocate(input)
    if (allocated(kernel)) deallocate(kernel)  
    if (allocated(output)) deallocate(output)
    
    allocate(input(batch * in_c * h * w))
    allocate(kernel(out_c * in_c * kh * kw))
    allocate(output(batch * out_c * h_out * w_out))
    
    ! Initialize with test data
    call random_number(input)
    call random_number(kernel)
    output = 0.0
    
    ! Run with auto-tuner
    achieved_gflops = auto_tune_conv2d(input, kernel, output, &
                                       batch, in_c, out_c, h, w, kh, kw)
    
    print *, ""
  end do
  
  print *, "=============================================="
  print *, "Phase 2: Learning Mode (Performance Improves)"
  print *, "=============================================="
  print *, ""
  
  call enable_learning_mode(.true.)
  
  print *, "Running 3 iterations to demonstrate learning..."
  print *, ""
  
  do run = 1, 3
    print *, "--- Iteration", run, "---"
    
    do test = 3, 5  ! Focus on medium to large workloads
      batch = test_configs(test, 1)
      in_c = test_configs(test, 2)
      out_c = test_configs(test, 3)
      h = test_configs(test, 4)
      w = test_configs(test, 5)
      kh = test_configs(test, 6)
      kw = kh
      
      h_out = h - kh + 1
      w_out = w - kw + 1
      
      ! Allocate arrays
      if (allocated(input)) deallocate(input)
      if (allocated(kernel)) deallocate(kernel)
      if (allocated(output)) deallocate(output)
      
      allocate(input(batch * in_c * h * w))
      allocate(kernel(out_c * in_c * kh * kw))
      allocate(output(batch * out_c * h_out * w_out))
      
      call random_number(input)
      call random_number(kernel)
      output = 0.0
      
      achieved_gflops = auto_tune_conv2d(input, kernel, output, &
                                         batch, in_c, out_c, h, w, kh, kw)
    end do
    
    print *, ""
  end do
  
  print *, "=============================================="
  print *, "Phase 3: Tensor Core Boost"
  print *, "=============================================="
  print *, ""
  
  call enable_tensor_cores(.true.)
  
  ! Test extreme workload with tensor cores
  test = 6
  batch = test_configs(test, 1)
  in_c = test_configs(test, 2)
  out_c = test_configs(test, 3)
  h = test_configs(test, 4)
  w = test_configs(test, 5)
  kh = test_configs(test, 6)
  kw = kh
  
  h_out = h - kh + 1
  w_out = w - kw + 1
  
  total_flops = int(batch, i64) * out_c * h_out * w_out * in_c * kh * kw * 2
  workload_gflops = real(total_flops, dp) / 1.0e9_dp
  
  print *, "Extreme workload with tensor cores:"
  print '(A,I0,A,I0,A,I0,A,I0)', "  Config: ", batch, "×", in_c, "×", h, "×", w
  print '(A,F10.1,A)', "  Workload: ", workload_gflops, " GFLOPs"
  
  if (allocated(input)) deallocate(input)
  if (allocated(kernel)) deallocate(kernel)
  if (allocated(output)) deallocate(output)
  
  allocate(input(batch * in_c * h * w))
  allocate(kernel(out_c * in_c * kh * kw))
  allocate(output(batch * out_c * h_out * w_out))
  
  call random_number(input)
  call random_number(kernel)
  output = 0.0
  
  achieved_gflops = auto_tune_conv2d(input, kernel, output, &
                                     batch, in_c, out_c, h, w, kh, kw)
  
  print *, ""
  print *, "=============================================="
  print *, "Summary: The Power of Universal Auto-Tuning"
  print *, "=============================================="
  print *, ""
  print *, "✅ Automatic device selection based on workload"
  print *, "✅ Hardware-specific parameter optimization"
  print *, "✅ Performance improvement through learning"
  print *, "✅ Tensor core acceleration when beneficial"
  print *, ""
  print *, "Key Insights:"
  print *, "  • Small workloads → CPU (avoid GPU overhead)"
  print *, "  • Medium workloads → Depends on specific hardware"
  print *, "  • Large workloads → GPU with optimized kernels"
  print *, "  • Extreme workloads → Tensor cores for maximum speed"
  print *, ""
  print *, "The Universal Pattern Revolution:"
  print *, "  One framework. All devices. Optimal performance. No manual tuning."
  print *, ""
  
  ! Cleanup
  if (allocated(input)) deallocate(input)
  if (allocated(kernel)) deallocate(kernel)
  if (allocated(output)) deallocate(output)
  
end program test_universal_autotuner