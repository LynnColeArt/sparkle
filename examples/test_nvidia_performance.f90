program test_nvidia_performance
  ! Benchmark NVIDIA A4500 performance with Sporkle
  ! Target: Beat AMD's 3,630 GFLOPS!
  
  use kinds
  use sporkle_nvidia_opengl
  use iso_c_binding
  implicit none
  
  ! Test parameters
  integer, parameter :: batch = 1
  integer, parameter :: in_channels = 3
  integer, parameter :: out_channels = 64
  integer, parameter :: height = 224
  integer, parameter :: width = 224
  integer, parameter :: kernel_h = 3
  integer, parameter :: kernel_w = 3
  
  ! Arrays
  real(sp), allocatable :: input(:), kernel(:), output(:)
  
  ! Timing
  real(dp) :: start_time, end_time, elapsed_ms
  real(dp) :: gflops
  integer :: i, num_iterations
  logical :: success
  character(len=256) :: device_info
  
  print *, "=============================================="
  print *, "Sporkle NVIDIA A4500 Performance Benchmark"
  print *, "=============================================="
  print *, ""
  
  ! Initialize NVIDIA OpenGL
  print *, "Initializing NVIDIA GPU..."
  success = nvidia_gl_init()
  if (.not. success) then
    print *, "Failed to initialize NVIDIA OpenGL"
    stop 1
  end if
  
  ! Get device info
  device_info = nvidia_gl_get_device_info()
  print *, "Device:", trim(device_info)
  print *, ""
  
  ! Allocate arrays
  allocate(input(batch * in_channels * height * width))
  allocate(kernel(out_channels * in_channels * kernel_h * kernel_w))
  allocate(output(batch * out_channels * height * width))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(kernel)
  
  print *, "Configuration:"
  print *, "  Input:", batch, "x", in_channels, "x", height, "x", width
  print *, "  Kernel:", out_channels, "x", in_channels, "x", kernel_h, "x", kernel_w
  print *, "  Output:", batch, "x", out_channels, "x", height, "x", width
  print *, ""
  
  ! Warm-up run
  print *, "Warming up..."
  success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_channels, out_channels, &
                                      height, width, kernel_h, kernel_w)
  if (.not. success) then
    print *, "Warm-up failed!"
    stop 1
  end if
  
  ! Benchmark runs
  print *, ""
  print *, "Running benchmark..."
  num_iterations = 10
  
  call cpu_time(start_time)
  do i = 1, num_iterations
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                        batch, in_channels, out_channels, &
                                        height, width, kernel_h, kernel_w)
    if (.not. success) then
      print *, "Iteration", i, "failed!"
      exit
    end if
  end do
  call cpu_time(end_time)
  
  ! Calculate performance
  elapsed_ms = (end_time - start_time) * 1000.0_dp / num_iterations
  
  ! FLOPs calculation for convolution
  ! Each output pixel: kernel_h * kernel_w * in_channels multiply-adds
  ! Total: batch * out_channels * height * width * kernel_h * kernel_w * in_channels * 2
  gflops = real(batch, dp) * real(out_channels, dp) * real(height, dp) * real(width, dp) * &
           real(kernel_h, dp) * real(kernel_w, dp) * real(in_channels, dp) * 2.0_dp
  gflops = gflops / (elapsed_ms * 1.0e6_dp)  ! Convert to GFLOPS
  
  print *, ""
  print *, "=============================================="
  print *, "BENCHMARK RESULTS:"
  print *, "=============================================="
  print *, "Average time per iteration:", elapsed_ms, "ms"
  print *, "Performance:", gflops, "GFLOPS"
  print *, ""
  
  if (gflops > 3630.0_dp) then
    print *, "🚀 NEW RECORD! Beat AMD's 3,630 GFLOPS!"
  else if (gflops > 1000.0_dp) then
    print *, "✨ Excellent! Over 1 TFLOPS achieved!"
  else if (gflops > 400.0_dp) then
    print *, "✓ Good performance, matching sync GPU levels"
  end if
  
  print *, "=============================================="
  
  ! Cleanup
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  
end program test_nvidia_performance