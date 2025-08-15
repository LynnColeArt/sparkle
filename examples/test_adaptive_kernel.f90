program test_adaptive_kernel
  use iso_c_binding
  use sparkle_types
  use sparkle_adaptive_kernel
  use sparkle_kernel_variants
  implicit none
  
  integer, parameter :: int64 = c_int64_t
  
  type(adaptive_kernel) :: conv_kernel
  integer :: selected_variant
  integer(int64) :: workload_sizes(5)
  integer :: i, j
  real :: start_time, end_time
  
  print *, "=== Testing Adaptive Kernel Framework ==="
  print *, ""
  
  ! Create adaptive kernel
  conv_kernel = create_adaptive_kernel("convolution_gemm")
  
  ! Add kernel variants
  call add_kernel_variant(conv_kernel, "GLSL", 1, execute_glsl_variant)
  call add_kernel_variant(conv_kernel, "SPIR-V", 2, execute_spirv_variant)
  call add_kernel_variant(conv_kernel, "Direct", 3, execute_direct_variant)
  
  print *, "Added", conv_kernel%num_variants, "kernel variants"
  print *, ""
  
  ! Test with different workload sizes
  workload_sizes = [int(1e5, int64), int(1e6, int64), int(1e7, int64), &
                    int(5e7, int64), int(1e8, int64)]
  
  do i = 1, size(workload_sizes)
    print *, "Testing with workload size:", workload_sizes(i)
    
    ! Select optimal variant (will trigger probing on first run or size change)
    selected_variant = select_optimal_variant(conv_kernel, workload_sizes(i))
    
    ! Run the selected variant a few times
    print *, "Running selected variant:", &
             trim(conv_kernel%variants(selected_variant)%name)
    
    do j = 1, 3
      call cpu_time(start_time)
      ! In real use, we'd execute the actual kernel here
      call cpu_time(end_time)
    end do
    
    print *, ""
  end do
  
  ! Print final statistics
  print *, "=== Final Variant Statistics ==="
  call print_variant_stats(conv_kernel)
  
  ! Demonstrate forcing a specific variant
  print *, "=== Testing Forced Variant Selection ==="
  call force_variant(conv_kernel, "SPIR-V")
  
  selected_variant = select_optimal_variant(conv_kernel, int(1e6, int64))
  print *, "After forcing: selected =", &
           trim(conv_kernel%variants(selected_variant)%name)
  print *, ""
  
  ! Test workload size change detection
  print *, "=== Testing Workload Size Change Detection ==="
  print *, "Drastically changing workload size..."
  
  ! This should trigger reprobing due to large size change
  selected_variant = select_optimal_variant(conv_kernel, int(1e3, int64))
  
  ! Clean up
  call cleanup_adaptive_kernel(conv_kernel)
  
  print *, "Test completed!"
  
end program test_adaptive_kernel