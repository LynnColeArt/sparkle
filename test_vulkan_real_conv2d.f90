program test_vulkan_real_conv2d
  ! Test Vulkan Backend with Real Conv2D Execution
  ! ==============================================
  !
  ! This test runs actual convolution kernels on Vulkan to measure
  ! real performance and break through the 2.6 TFLOPS ceiling.
  
  use kinds
  use iso_c_binding
  use gpu_vulkan_interface
  use flopcount, only: conv2d_flops
  implicit none
  
  ! C interfaces are now in gpu_vulkan_interface module
  
  ! Test parameters - same as OpenGL benchmark
  integer, parameter :: N = 1
  integer, parameter :: C = 64
  integer, parameter :: H = 224
  integer, parameter :: W = 224
  integer, parameter :: K = 64
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  
  ! Arrays and buffers
  real(sp), allocatable :: input(:,:,:,:)
  real(sp), allocatable :: weights(:,:,:,:)
  real(sp), allocatable :: output(:,:,:,:)
  type(c_ptr) :: input_buf, weights_buf, output_buf
  type(c_ptr) :: shader
  
  ! Timing and performance
  real(sp) :: elapsed_ms, gflops
  real(dp) :: flops
  integer :: H_out, W_out
  integer :: i, warmup_runs, test_runs
  real(sp) :: total_time, min_time, max_time
  real(sp) :: avg_time, avg_gflops, best_gflops
  integer(i64) :: input_size, weights_size, output_size
  
  print *, "🚀 Vulkan Real Conv2D Performance Test"
  print *, "====================================="
  print *, ""
  
  ! Initialize Vulkan
  if (.not. gpu_init_vulkan()) then
    print *, "❌ Failed to initialize Vulkan"
    stop 1
  end if
  
  ! Create and compile conv2d shader
  print *, "📄 Creating optimized conv2d shader..."
  shader = vk_create_conv2d_shader()
  if (.not. c_associated(shader)) then
    print *, "❌ Failed to create conv2d shader"
    call gpu_cleanup_vulkan()
    stop 1
  end if
  print *, "✅ Conv2d shader compiled successfully!"
  print *, ""
  
  ! Calculate output dimensions
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Allocate CPU arrays
  allocate(input(C, H, W, N))
  allocate(weights(C, kernel_size, kernel_size, K))
  allocate(output(K, H_out, W_out, N))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  output = 0.0_sp
  
  ! Calculate sizes
  input_size = int(N, i64) * int(C, i64) * int(H, i64) * int(W, i64) * 4_i64
  weights_size = int(K, i64) * int(C, i64) * int(kernel_size, i64) * int(kernel_size, i64) * 4_i64
  output_size = int(N, i64) * int(K, i64) * int(H_out, i64) * int(W_out, i64) * 4_i64
  
  print '(A)', "📊 Workload Configuration:"
  print '(A,I0,A,I0,A,I0,A,I0)', "   Input:  N=", N, ", C=", C, ", H=", H, ", W=", W
  print '(A,I0,A,I0,A)', "   Filter: ", kernel_size, "x", kernel_size, " (64 -> 64 channels)"
  print '(A,I0,A,I0)', "   Output: H=", H_out, ", W=", W_out
  print '(A,F0.2,A)', "   Memory: ", real(input_size + weights_size + output_size) / 1e6_dp, " MB total"
  print *, ""
  
  ! Allocate DEVICE_LOCAL buffers
  print *, "🎯 Allocating DEVICE_LOCAL memory (true VRAM):"
  input_buf = gpu_allocate_buffer_vulkan(input_size, .true.)
  weights_buf = gpu_allocate_buffer_vulkan(weights_size, .true.)
  output_buf = gpu_allocate_buffer_vulkan(output_size, .true.)
  
  if (.not. c_associated(input_buf) .or. &
      .not. c_associated(weights_buf) .or. &
      .not. c_associated(output_buf)) then
    print *, "❌ Failed to allocate Vulkan buffers"
    call gpu_cleanup_vulkan()
    stop 1
  end if
  
  print *, ""
  print *, "✅ All buffers allocated in VRAM!"
  print *, ""
  
  ! TODO: Copy data to GPU buffers (needs staging buffer implementation)
  print *, "⚠️  Note: Data transfer not yet implemented"
  print *, "   Running with uninitialized GPU buffers for timing only"
  print *, ""
  
  ! Calculate FLOPS
  flops = conv2d_flops(int(N,i64), int(C,i64), int(H,i64), int(W,i64), &
                      int(K,i64), int(kernel_size,i64), int(kernel_size,i64))
  
  ! Warmup runs
  warmup_runs = 5
  test_runs = 20
  
  print *, "🔥 Warmup runs..."
  do i = 1, warmup_runs
    elapsed_ms = vk_dispatch_conv2d(shader, input_buf, weights_buf, output_buf, &
                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (elapsed_ms <= 0.0) then
      print *, "❌ Dispatch failed during warmup"
      exit
    end if
  end do
  
  ! Performance test
  print *, ""
  print *, "📈 Performance Test Results:"
  print *, "=========================="
  
  total_time = 0.0_sp
  min_time = 1e6_sp
  max_time = 0.0_sp
  
  do i = 1, test_runs
    elapsed_ms = vk_dispatch_conv2d(shader, input_buf, weights_buf, output_buf, &
                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    if (elapsed_ms <= 0.0) then
      print *, "❌ Dispatch failed during test"
      exit
    end if
    
    gflops = real(flops / elapsed_ms / 1e6_dp, sp)
    
    total_time = total_time + elapsed_ms
    min_time = min(min_time, elapsed_ms)
    max_time = max(max_time, elapsed_ms)
    
    if (mod(i, 5) == 0) then
      print '(A,I2,A,F8.2,A,F8.0,A)', "   Run ", i, ": ", elapsed_ms, " ms (", gflops, " GFLOPS)"
    end if
  end do
  
  ! Calculate statistics
  avg_time = total_time / real(test_runs, sp)
  avg_gflops = real(flops / avg_time / 1e6_dp, sp)
  best_gflops = real(flops / min_time / 1e6_dp, sp)
  
  print *, ""
  print *, "📊 Performance Summary:"
  print '(A,F8.2,A)', "   Average time: ", avg_time, " ms"
  print '(A,F8.0,A)', "   Average performance: ", avg_gflops, " GFLOPS"
  print '(A,F8.0,A)', "   Best performance: ", best_gflops, " GFLOPS"
  
  print *, ""
  print *, "🎯 Performance Analysis:"
  print '(A,F0.1,A)', "   OpenGL ceiling: 2,600 GFLOPS (", 2600.0_sp / 40000.0_sp * 100.0_sp, "% efficiency)"
  print '(A,F0.0,A,F0.1,A)', "   Vulkan actual: ", avg_gflops, " GFLOPS (", avg_gflops / 40000.0_sp * 100.0_sp, "% efficiency)"
  
  if (avg_gflops > 2600.0_sp) then
    print '(A,F0.1,A)', "   🚀 Performance improvement: ", avg_gflops / 2600.0_sp, "× over OpenGL!"
    print *, "   ✅ Successfully broke through the 2.6 TFLOPS ceiling!"
  else
    print *, "   ⚠️  Performance not yet optimized - check shader compilation"
  end if
  
  ! Cleanup
  call gpu_free_buffer_vulkan(input_buf)
  call gpu_free_buffer_vulkan(weights_buf)
  call gpu_free_buffer_vulkan(output_buf)
  ! TODO: Add vk_free_shader when implemented
  call gpu_cleanup_vulkan()
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "🎉 Test complete!"
  
end program test_vulkan_real_conv2d