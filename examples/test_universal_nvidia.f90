program test_universal_nvidia
  ! Test NVIDIA implementation with true universal patterns
  
  use kinds
  use sporkle_hardware_profiler
  use sporkle_nvidia_universal
  implicit none
  
  ! Test parameters
  integer, parameter :: batch = 1
  integer, parameter :: in_channels = 3
  integer, parameter :: out_channels = 64
  integer, parameter :: height = 224
  integer, parameter :: width = 224
  integer, parameter :: kernel_size = 3
  
  ! Arrays
  real(sp), allocatable :: input(:,:,:,:)
  real(sp), allocatable :: kernel(:,:,:,:)
  real(sp), allocatable :: output(:,:,:,:)
  
  ! Output dimensions
  integer :: h_out, w_out
  
  ! Performance metrics
  real(dp) :: gflops
  integer(i64) :: total_flops
  
  ! Calculate output dimensions
  h_out = height - kernel_size + 1
  w_out = width - kernel_size + 1
  
  ! Allocate arrays
  allocate(input(batch, in_channels, height, width))
  allocate(kernel(out_channels, in_channels, kernel_size, kernel_size))
  allocate(output(batch, out_channels, h_out, w_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(kernel)
  output = 0.0_sp
  
  print *, "=============================================="
  print *, "NVIDIA Universal Pattern Test"
  print *, "=============================================="
  print *, ""
  print *, "Problem size:"
  print '(A,I0,A,I0,A,I0,A,I0)', "  Input: ", batch, "×", in_channels, "×", height, "×", width
  print '(A,I0,A,I0,A,I0,A,I0)', "  Kernel: ", out_channels, "×", in_channels, "×", kernel_size, "×", kernel_size
  print '(A,I0,A,I0,A,I0,A,I0)', "  Output: ", batch, "×", out_channels, "×", h_out, "×", w_out
  print *, ""
  
  ! Calculate theoretical FLOPs
  total_flops = int(batch, i64) * out_channels * h_out * w_out * in_channels * kernel_size * kernel_size * 2
  print '(A,F10.2,A)', "Total FLOPs: ", real(total_flops) / 1.0e9, " GFLOPS"
  print *, ""
  
  ! Initialize the universal pattern system
  call nvidia_universal_init()
  
  print *, "Running convolution with universal patterns..."
  print *, ""
  
  ! Run the optimized convolution
  gflops = nvidia_universal_conv2d(input, kernel, output, &
                                   batch, in_channels, out_channels, &
                                   height, width, kernel_size, kernel_size)
  
  print *, ""
  print *, "=============================================="
  print *, "Performance Analysis"
  print *, "=============================================="
  
  ! Compare with current and target performance
  block
    type(hardware_characteristics) :: hw
    type(kernel_parameters) :: params
    real(dp) :: current_gflops, target_gflops, efficiency
    
    ! Get hardware profile
    hw = profile_nvidia_gpu()
    params = derive_optimal_parameters(hw)
    
    current_gflops = 113.0_dp  ! Current baseline
    target_gflops = hw%peak_gflops * 0.70_dp  ! 70% efficiency target
    
    print '(A,F10.1,A)', "Current baseline: ", current_gflops, " GFLOPS"
    print '(A,F10.1,A)', "Universal pattern projection: ", gflops, " GFLOPS"
    print '(A,F10.1,A)', "Target (70% efficiency): ", target_gflops, " GFLOPS"
    print '(A,F10.1,A)', "Theoretical peak: ", hw%peak_gflops, " GFLOPS"
    print *, ""
    
    if (gflops > 0.0_dp) then
      efficiency = gflops / hw%peak_gflops * 100.0_dp
      print '(A,F6.2,A)', "Achieved efficiency: ", efficiency, "%"
      print '(A,F6.1,A)', "Speedup over baseline: ", gflops / current_gflops, "×"
    end if
    
    print *, ""
    print *, "Key optimizations applied:"
    print '(A,I0,A)', "  • Block size: ", params%block_size, " threads (vs 256 baseline)"
    print '(A,I0,A,I0)', "  • Tile size: ", params%tile_size, "×", params%tile_size, " (vs 16×16 baseline)"
    print '(A,I0,A,I0)', "  • Outputs per thread: ", params%outputs_per_thread, "×", params%outputs_per_thread
    print '(A,I0,A)', "  • Unroll factor: ", params%unroll_factor, "× (vs 1× baseline)"
    print '(A,L1)', "  • Shared memory: ", params%use_shared_memory
    
  end block
  
  print *, ""
  print *, "=============================================="
  print *, "Next Steps"
  print *, "=============================================="
  print *, "1. Implement OpenGL compute shader with these parameters"
  print *, "2. Use shared memory for tile-based computation"
  print *, "3. Apply aggressive unrolling and prefetching"
  print *, "4. Achieve 16+ TFLOPS real performance"
  print *, ""
  
  ! Cleanup
  call nvidia_universal_cleanup()
  deallocate(input, kernel, output)
  
end program test_universal_nvidia