program test_vulkan_real_performance
  ! Test Vulkan with Real Conv2D Performance
  ! =======================================
  !
  ! Measures actual convolution performance with proper GPU timing
  
  use kinds
  use iso_c_binding
  use gpu_vulkan_interface
  use flopcount, only: conv2d_flops
  implicit none
  
  ! C interfaces
  interface
    function vk_allocate_buffer_with_staging(size_bytes, device_local) &
             bind(C, name="vk_allocate_buffer_with_staging")
      import :: c_size_t, c_int, c_ptr
      integer(c_size_t), value :: size_bytes
      integer(c_int), value :: device_local
      type(c_ptr) :: vk_allocate_buffer_with_staging
    end function
    
    subroutine vk_upload_buffer_data(buffer, data, size) &
               bind(C, name="vk_upload_buffer_data")
      import :: c_ptr, c_size_t
      type(c_ptr), value :: buffer
      type(c_ptr), value :: data
      integer(c_size_t), value :: size
    end subroutine
    
    subroutine vk_free_buffer_full(buffer) bind(C, name="vk_free_buffer_full")
      import :: c_ptr
      type(c_ptr), value :: buffer
    end subroutine
    
    function vk_init_timing() bind(C, name="vk_init_timing")
      import :: c_int
      integer(c_int) :: vk_init_timing
    end function
    
    subroutine vk_cleanup_timing() bind(C, name="vk_cleanup_timing")
    end subroutine
    
    function vk_create_conv2d_shader_real() bind(C, name="vk_create_conv2d_shader_real")
      import :: c_ptr
      type(c_ptr) :: vk_create_conv2d_shader_real
    end function
    
    function vk_dispatch_conv2d_timed(shader, input_buf, weights_buf, output_buf, &
                                     N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
                                     query_base) bind(C, name="vk_dispatch_conv2d_timed")
      import :: c_ptr, c_int, c_float
      type(c_ptr), value :: shader, input_buf, weights_buf, output_buf
      integer(c_int), value :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
      integer(c_int), value :: query_base
      real(c_float) :: vk_dispatch_conv2d_timed
    end function
  end interface
  
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
  real(sp), allocatable, target :: input(:,:,:,:)
  real(sp), allocatable, target :: weights(:,:,:,:)
  real(sp), allocatable, target :: output(:,:,:,:)
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
  integer :: query_base
  
  print *, "🎯 Vulkan Real Performance Test"
  print *, "==============================="
  print *, ""
  
  ! Initialize Vulkan
  if (.not. gpu_init_vulkan()) then
    print *, "❌ Failed to initialize Vulkan"
    stop 1
  end if
  
  ! Initialize GPU timing
  if (vk_init_timing() == 0) then
    print *, "❌ Failed to initialize GPU timing"
    call gpu_cleanup_vulkan()
    stop 1
  end if
  
  ! Create real conv2d shader
  print *, "📄 Creating real conv2d compute shader..."
  shader = vk_create_conv2d_shader_real()
  if (.not. c_associated(shader)) then
    print *, "❌ Failed to create conv2d shader"
    call vk_cleanup_timing()
    call gpu_cleanup_vulkan()
    stop 1
  end if
  print *, "✅ Real conv2d shader created!"
  print *, ""
  
  ! Calculate output dimensions
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Allocate CPU arrays
  allocate(input(C, H, W, N))
  allocate(weights(C, kernel_size, kernel_size, K))
  allocate(output(K, H_out, W_out, N))
  
  ! Initialize with real data
  call random_number(input)
  call random_number(weights)
  input = input - 0.5_sp     ! Center around zero
  weights = weights * 0.1_sp  ! Small weights to avoid overflow
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
  
  ! Allocate GPU buffers with staging
  print *, "🎯 Allocating GPU memory with staging..."
  input_buf = vk_allocate_buffer_with_staging(input_size, 1)
  weights_buf = vk_allocate_buffer_with_staging(weights_size, 1)
  output_buf = vk_allocate_buffer_with_staging(output_size, 1)
  
  if (.not. c_associated(input_buf) .or. &
      .not. c_associated(weights_buf) .or. &
      .not. c_associated(output_buf)) then
    print *, "❌ Failed to allocate GPU buffers"
    call vk_cleanup_timing()
    call gpu_cleanup_vulkan()
    stop 1
  end if
  
  ! Upload data to GPU
  print *, "📤 Uploading data to GPU..."
  call vk_upload_buffer_data(input_buf, c_loc(input), input_size)
  call vk_upload_buffer_data(weights_buf, c_loc(weights), weights_size)
  call vk_upload_buffer_data(output_buf, c_loc(output), output_size)
  
  print *, "✅ GPU memory initialized with real data!"
  print *, ""
  
  ! Calculate FLOPS
  flops = conv2d_flops(int(N,i64), int(C,i64), int(H,i64), int(W,i64), &
                      int(K,i64), int(kernel_size,i64), int(kernel_size,i64))
  
  ! Warmup runs
  warmup_runs = 10
  test_runs = 50
  query_base = 0
  
  print *, "🔥 Warmup runs..."
  do i = 1, warmup_runs
    elapsed_ms = vk_dispatch_conv2d_timed(shader, input_buf, weights_buf, output_buf, &
                                         N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
                                         query_base)
    query_base = query_base + 2
    if (elapsed_ms <= 0.0) then
      print *, "❌ Dispatch failed during warmup"
      exit
    end if
  end do
  
  ! Performance test
  print *, ""
  print *, "📈 Real Performance Test Results:"
  print *, "================================"
  
  total_time = 0.0_sp
  min_time = 1e6_sp
  max_time = 0.0_sp
  
  do i = 1, test_runs
    elapsed_ms = vk_dispatch_conv2d_timed(shader, input_buf, weights_buf, output_buf, &
                                         N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
                                         query_base)
    query_base = query_base + 2
    
    if (elapsed_ms <= 0.0) then
      print *, "❌ Dispatch failed during test"
      exit
    end if
    
    gflops = real(flops / elapsed_ms / 1e6_dp, sp)
    
    total_time = total_time + elapsed_ms
    min_time = min(min_time, elapsed_ms)
    max_time = max(max_time, elapsed_ms)
    
    if (mod(i, 10) == 0) then
      print '(A,I2,A,F8.3,A,F8.0,A)', "   Run ", i, ": ", elapsed_ms, " ms (", gflops, " GFLOPS)"
    end if
  end do
  
  ! Calculate statistics
  avg_time = total_time / real(test_runs, sp)
  avg_gflops = real(flops / avg_time / 1e6_dp, sp)
  best_gflops = real(flops / min_time / 1e6_dp, sp)
  
  print *, ""
  print *, "📊 Performance Summary:"
  print '(A,F8.3,A)', "   Average time: ", avg_time, " ms"
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
    print *, "   ⚠️  Performance not yet optimized"
  end if
  
  ! Cleanup
  call vk_free_buffer_full(input_buf)
  call vk_free_buffer_full(weights_buf)
  call vk_free_buffer_full(output_buf)
  ! TODO: Add vk_free_shader when implemented
  call vk_cleanup_timing()
  call gpu_cleanup_vulkan()
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "🎉 Real performance test complete!"
  
end program test_vulkan_real_performance