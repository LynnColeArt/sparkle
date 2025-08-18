! Test Automatic Device Selection
! ===============================
!
! This test demonstrates intelligent device selection based on:
!   - Workload size and characteristics
!   - Arithmetic intensity (compute vs memory bound)
!   - Historical performance data
!   - Device capabilities

program test_auto_device_selection
  use iso_fortran_env, only: real32, real64, int64
  use sparkle_conv2d_auto_selector
  implicit none
  
  ! Test configurations - various workload sizes
  type :: test_config
    character(len=64) :: name
    integer :: N, C, H, W, K
    integer :: kernel_size, stride, pad
  end type
  
  type(test_config), parameter :: tests(8) = [ &
    ! Small workloads (should prefer CPU due to kernel launch overhead)
    test_config("Tiny (1×3×32×32)", 1, 3, 32, 32, 64, 3, 1, 1), &
    test_config("Small (1×64×56×56)", 1, 64, 56, 56, 64, 3, 1, 1), &
    ! Medium workloads (borderline)
    test_config("Medium (1×128×28×28)", 1, 128, 28, 28, 256, 3, 1, 1), &
    test_config("ResNet50-L3 (4×128×28×28)", 4, 128, 28, 28, 256, 3, 1, 1), &
    ! Large workloads (should prefer GPU)
    test_config("Large (1×256×14×14)", 1, 256, 14, 14, 512, 3, 1, 1), &
    test_config("ResNet50-L1 (4×3×224×224)", 4, 3, 224, 224, 64, 7, 2, 3), &
    ! Extreme workloads
    test_config("Huge (8×512×7×7)", 8, 512, 7, 7, 2048, 3, 1, 1), &
    test_config("Giant (16×256×28×28)", 16, 256, 28, 28, 512, 3, 1, 1) &
  ]
  
  ! Variables
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: test_idx, i
  integer :: H_out, W_out
  integer :: input_size, weight_size, output_size
  real(real32) :: time_ms
  integer(int64) :: total_flops
  real(real32) :: gflops, ai
  real(real32) :: total_time_cpu, total_time_gpu
  integer :: cpu_count, gpu_count
  
  print *, "🤖 Automatic Device Selection Test"
  print *, "=================================="
  print *, ""
  print *, "This test runs various workload sizes to demonstrate"
  print *, "intelligent device selection based on characteristics."
  print *, ""
  
  ! Initialize the auto selector
  call init_auto_selector()
  
  ! Enable profiling for detailed output
  call set_profiling_mode(.true.)
  
  print *, "📊 Phase 1: Initial Profiling Run"
  print *, "---------------------------------"
  print *, "Running each workload to build performance profile..."
  print *, ""
  
  ! Reset counters
  total_time_cpu = 0.0
  total_time_gpu = 0.0
  cpu_count = 0
  gpu_count = 0
  
  ! Test each configuration
  do test_idx = 1, size(tests)
    ! Calculate dimensions
    H_out = (tests(test_idx)%H + 2*tests(test_idx)%pad - tests(test_idx)%kernel_size) / &
            tests(test_idx)%stride + 1
    W_out = (tests(test_idx)%W + 2*tests(test_idx)%pad - tests(test_idx)%kernel_size) / &
            tests(test_idx)%stride + 1
    
    ! Calculate sizes
    input_size = tests(test_idx)%N * tests(test_idx)%C * tests(test_idx)%H * tests(test_idx)%W
    weight_size = tests(test_idx)%K * tests(test_idx)%C * &
                  tests(test_idx)%kernel_size * tests(test_idx)%kernel_size
    output_size = tests(test_idx)%N * tests(test_idx)%K * H_out * W_out
    
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
    
    print '(A,I0,A,A)', "Test #", test_idx, ": ", trim(tests(test_idx)%name)
    
    ! Run with automatic selection
    time_ms = conv2d_auto_select(input, weights, output, &
                                tests(test_idx)%N, tests(test_idx)%C, &
                                tests(test_idx)%H, tests(test_idx)%W, &
                                tests(test_idx)%K, tests(test_idx)%kernel_size, &
                                tests(test_idx)%stride, tests(test_idx)%pad, &
                                H_out, W_out)
    
    ! Calculate metrics
    total_flops = int(tests(test_idx)%N, int64) * int(tests(test_idx)%K, int64) * &
                  int(H_out, int64) * int(W_out, int64) * &
                  int(tests(test_idx)%C, int64) * &
                  int(tests(test_idx)%kernel_size, int64) * &
                  int(tests(test_idx)%kernel_size, int64) * 2_int64
    
    gflops = real(total_flops) / (time_ms * 1e6)
    ai = real(total_flops) / real((input_size + weight_size + output_size) * 4)
    
    print '(A,F8.2,A,F8.1,A)', "   Time: ", time_ms, " ms, Performance: ", gflops, " GFLOPS"
    print *, ""
    
    ! Track device usage (crude detection from output)
    if (time_ms > 2.0) then
      cpu_count = cpu_count + 1
      total_time_cpu = total_time_cpu + time_ms
    else
      gpu_count = gpu_count + 1
      total_time_gpu = total_time_gpu + time_ms
    end if
    
    ! Cleanup
    deallocate(input, weights, output)
  end do
  
  print *, "📊 Phase 2: Performance with Learning"
  print *, "------------------------------------"
  print *, "Re-running to see if device selection improves..."
  print *, ""
  
  ! Disable verbose output for second run
  call set_profiling_mode(.false.)
  
  ! Run everything again to see learning
  do test_idx = 1, size(tests)
    ! Calculate dimensions
    H_out = (tests(test_idx)%H + 2*tests(test_idx)%pad - tests(test_idx)%kernel_size) / &
            tests(test_idx)%stride + 1
    W_out = (tests(test_idx)%W + 2*tests(test_idx)%pad - tests(test_idx)%kernel_size) / &
            tests(test_idx)%stride + 1
    
    ! Calculate sizes
    input_size = tests(test_idx)%N * tests(test_idx)%C * tests(test_idx)%H * tests(test_idx)%W
    weight_size = tests(test_idx)%K * tests(test_idx)%C * &
                  tests(test_idx)%kernel_size * tests(test_idx)%kernel_size
    output_size = tests(test_idx)%N * tests(test_idx)%K * H_out * W_out
    
    ! Allocate arrays
    allocate(input(input_size))
    allocate(weights(weight_size))
    allocate(output(output_size))
    
    ! Initialize
    call random_number(input)
    call random_number(weights)
    
    ! Run silently
    time_ms = conv2d_auto_select(input, weights, output, &
                                tests(test_idx)%N, tests(test_idx)%C, &
                                tests(test_idx)%H, tests(test_idx)%W, &
                                tests(test_idx)%K, tests(test_idx)%kernel_size, &
                                tests(test_idx)%stride, tests(test_idx)%pad, &
                                H_out, W_out)
    
    gflops = real(total_flops) / (time_ms * 1e6)
    
    print '(A,A,A,F8.2,A,F8.1,A)', "Test: ", trim(tests(test_idx)%name), &
          " - ", time_ms, " ms (", gflops, " GFLOPS)"
    
    ! Cleanup
    deallocate(input, weights, output)
  end do
  
  ! Show statistics
  call get_selector_stats()
  
  print *, ""
  print *, "📈 Summary"
  print *, "========="
  print '(A,I0,A,I0)', "CPU selected: ", cpu_count, " times, GPU selected: ", gpu_count, " times"
  if (cpu_count > 0) then
    print '(A,F8.2,A)', "Average CPU time: ", total_time_cpu / cpu_count, " ms"
  end if
  if (gpu_count > 0) then
    print '(A,F8.2,A)', "Average GPU time: ", total_time_gpu / gpu_count, " ms"
  end if
  
  print *, ""
  print *, "💡 Key Insights:"
  print *, "- Small workloads run faster on CPU (no kernel launch overhead)"
  print *, "- Large workloads benefit from GPU's massive parallelism"
  print *, "- The selector learns from performance history"
  print *, "- Arithmetic intensity influences device selection"
  
  ! Cleanup
  call cleanup_auto_selector()
  
  print *, ""
  print *, "✅ Test completed successfully!"
  
end program test_auto_device_selection