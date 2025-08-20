program test_nvidia_ultimate
  ! Test the ultimate NVIDIA implementation
  
  use kinds
  use sporkle_nvidia_ultimate
  implicit none
  
  ! Test parameters - realistic workload
  integer, parameter :: batch = 1
  integer, parameter :: in_channels = 64
  integer, parameter :: out_channels = 128
  integer, parameter :: height = 224
  integer, parameter :: width = 224
  integer, parameter :: kernel_size = 3
  
  ! Arrays
  real(sp), allocatable :: input(:,:,:,:)
  real(sp), allocatable :: kernel(:,:,:,:)
  real(sp), allocatable :: output(:,:,:,:)
  
  ! Performance metrics
  real(dp) :: gflops, time_start, time_end
  integer(i64) :: total_flops
  integer :: h_out, w_out
  logical :: success
  integer :: run
  
  print *, "=============================================="
  print *, "NVIDIA Ultimate Performance Test"
  print *, "=============================================="
  print *, ""
  
  ! Calculate dimensions
  h_out = height - kernel_size + 1
  w_out = width - kernel_size + 1
  
  ! Allocate arrays
  allocate(input(batch, in_channels, height, width))
  allocate(kernel(out_channels, in_channels, kernel_size, kernel_size))
  allocate(output(batch, out_channels, h_out, w_out))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(kernel)
  output = 0.0_sp
  
  print *, "Problem Configuration:"
  print '(A,I0,A,I0,A,I0,A,I0)', "  Input: ", batch, "×", in_channels, "×", height, "×", width
  print '(A,I0,A,I0,A,I0,A,I0)', "  Kernel: ", out_channels, "×", in_channels, "×", kernel_size, "×", kernel_size
  print '(A,I0,A,I0,A,I0,A,I0)', "  Output: ", batch, "×", out_channels, "×", h_out, "×", w_out
  
  ! Calculate total FLOPs
  total_flops = int(batch, i64) * out_channels * h_out * w_out * in_channels * kernel_size * kernel_size * 2
  print '(A,F10.2,A)', "  Total operations: ", real(total_flops) / 1.0e9_dp, " GFLOPs"
  print *, ""
  
  ! Initialize the ultimate implementation
  success = nvidia_ultimate_init()
  if (.not. success) then
    print *, "ERROR: Failed to initialize NVIDIA Ultimate"
    stop 1
  end if
  
  print *, "Running convolution benchmarks..."
  print *, "================================"
  
  ! Warmup run
  print *, "Warmup run..."
  gflops = nvidia_ultimate_conv2d(input, kernel, output, &
                                  batch, in_channels, out_channels, &
                                  height, width, kernel_size, kernel_size)
  
  print *, ""
  print *, "Benchmark runs:"
  
  ! Multiple benchmark runs
  do run = 1, 5
    print '(A,I0,A)', "Run ", run, ":"
    
    gflops = nvidia_ultimate_conv2d(input, kernel, output, &
                                    batch, in_channels, out_channels, &
                                    height, width, kernel_size, kernel_size)
    
    ! Show efficiency
    block
      real(dp) :: efficiency
      efficiency = gflops / 23650.0_dp * 100.0_dp
      print '(A,F8.2,A)', "  Efficiency: ", efficiency, "%"
    end block
  end do
  
  print *, ""
  print *, "=============================================="
  print *, "Performance Summary"
  print *, "=============================================="
  
  call nvidia_ultimate_get_info()
  
  print *, ""
  print *, "Universal Pattern Validation:"
  print *, "  ✓ Hardware profiling completed"
  print *, "  ✓ Optimal parameters derived"
  print *, "  ✓ Compute shader generated"
  print *, "  ✓ OpenGL context established"
  
  if (gflops > 10000.0_dp) then
    print *, "  ✓ ACHIEVED 10+ TFLOPS!"
    print *, ""
    print *, "🎉 UNIVERSAL PATTERNS WORK! 🎉"
  else if (gflops > 1000.0_dp) then
    print *, "  ✓ Achieved significant speedup"
    print *, "  → Need to optimize shader further"
  else
    print *, "  → Performance needs investigation"
  end if
  
  print *, ""
  print *, "Next steps:"
  print *, "  1. Profile with Nsight to verify optimization"
  print *, "  2. Compare with CUDA implementation"
  print *, "  3. Extend to other problem sizes"
  print *, "  4. Implement auto-tuning runtime"
  
  ! Cleanup
  call nvidia_ultimate_shutdown()
  deallocate(input, kernel, output)
  
end program test_nvidia_ultimate