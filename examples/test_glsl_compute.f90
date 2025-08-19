program test_glsl_compute
  use iso_c_binding
  use sporkle_types
  use sporkle_glsl_generator
  use sporkle_glsl_compute
  use sporkle_amdgpu_direct
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: input_buffer, kernel_buffer, output_buffer
  type(convolution_config) :: config
  type(glsl_kernel) :: kernel
  integer :: status
  integer :: M, N, K
  real, allocatable :: input_data(:), kernel_data(:), output_data(:)
  real :: start_time, end_time
  integer :: i
  
  print *, "=== Testing GLSL Compute Shader Implementation ==="
  
  ! Initialize OpenGL compute context
  status = init_opengl_compute()
  if (status /= 0) then
    print *, "Failed to initialize OpenGL compute context"
    stop 1
  end if
  
  ! Set up convolution configuration
  config%input_height = 224
  config%input_width = 224
  config%input_channels = 3
  config%kernel_height = 3
  config%kernel_width = 3
  config%output_height = 222  ! (224 - 3 + 1)
  config%output_width = 222
  config%output_channels = 64
  config%stride_y = 1
  config%stride_x = 1
  config%pad_y = 0
  config%pad_x = 0
  config%tile_size = 16
  config%use_fp16 = .false.
  
  ! Calculate matrix dimensions for GEMM
  M = config%output_height * config%output_width  ! 49284
  N = config%output_channels                      ! 64
  K = config%kernel_height * config%kernel_width * config%input_channels  ! 27
  
  print *, "Convolution as GEMM dimensions:"
  print *, "  M (output spatial) =", M
  print *, "  N (output channels) =", N
  print *, "  K (kernel size * input channels) =", K
  
  ! Generate and compile shader
  print *, ""
  print *, "Generating GLSL compute shader..."
  kernel = compile_glsl_kernel(config)
  
  if (.not. kernel%is_valid) then
    print *, "ERROR: Failed to compile GLSL kernel"
    stop 1
  end if
  
  print *, "Shader compiled successfully!"
  
  ! Initialize AMDGPU device (for buffer management)
  status = amdgpu_init_device(device, 0)
  if (status /= 0) then
    print *, "Failed to initialize AMDGPU device"
    call cleanup_glsl_kernel(kernel)
    stop 1
  end if
  
  ! Allocate buffers
  print *, ""
  print *, "Allocating GPU buffers..."
  
  ! Input buffer (im2col transformed)
  status = amdgpu_allocate_buffer(device, input_buffer, int(M * K * 4, c_int64_t))
  if (status /= 0) then
    print *, "Failed to allocate input buffer"
    call cleanup_glsl_kernel(kernel)
    call amdgpu_cleanup_device(device)
    stop 1
  end if
  
  ! Kernel buffer
  status = amdgpu_allocate_buffer(device, kernel_buffer, int(K * N * 4, c_int64_t))
  if (status /= 0) then
    print *, "Failed to allocate kernel buffer"
    call amdgpu_free_buffer(device, input_buffer)
    call cleanup_glsl_kernel(kernel)
    call amdgpu_cleanup_device(device)
    stop 1
  end if
  
  ! Output buffer
  status = amdgpu_allocate_buffer(device, output_buffer, int(M * N * 4, c_int64_t))
  if (status /= 0) then
    print *, "Failed to allocate output buffer"
    call amdgpu_free_buffer(device, input_buffer)
    call amdgpu_free_buffer(device, kernel_buffer)
    call cleanup_glsl_kernel(kernel)
    call amdgpu_cleanup_device(device)
    stop 1
  end if
  
  ! Initialize test data
  print *, "Initializing test data..."
  allocate(input_data(M * K))
  allocate(kernel_data(K * N))
  allocate(output_data(M * N))
  
  ! Fill with test pattern
  do i = 1, M * K
    input_data(i) = real(mod(i, 255)) / 255.0
  end do
  
  do i = 1, K * N
    kernel_data(i) = real(mod(i, 127)) / 127.0
  end do
  
  ! Copy data to GPU buffers
  ! Note: In real implementation, we'd copy data to GL buffer objects
  print *, "Copying data to GPU..."
  
  ! Run kernel
  print *, ""
  print *, "Dispatching GLSL compute shader..."
  
  call cpu_time(start_time)
  call dispatch_conv_glsl(kernel, input_buffer, kernel_buffer, output_buffer, M, N, K)
  call cpu_time(end_time)
  
  print *, "Kernel execution time:", (end_time - start_time) * 1000.0, "ms"
  
  ! Calculate theoretical FLOPS
  ! Each output element requires K multiply-add operations
  ! Total operations = M * N * K * 2
  real :: total_ops, gflops
  total_ops = real(M) * real(N) * real(K) * 2.0
  gflops = total_ops / ((end_time - start_time) * 1.0e9)
  
  print *, ""
  print *, "Performance metrics:"
  print *, "  Total operations:", total_ops / 1.0e9, "GFLOPS"
  print *, "  Achieved performance:", gflops, "GFLOPS"
  
  ! Verify first few outputs (in real implementation)
  print *, ""
  print *, "Note: This is a prototype implementation."
  print *, "Full GL buffer integration pending."
  
  ! Cleanup
  print *, ""
  print *, "Cleaning up..."
  deallocate(input_data, kernel_data, output_data)
  
  call amdgpu_free_buffer(device, input_buffer)
  call amdgpu_free_buffer(device, kernel_buffer)
  call amdgpu_free_buffer(device, output_buffer)
  
  call cleanup_glsl_kernel(kernel)
  call amdgpu_cleanup_device(device)
  call cleanup_opengl_compute()
  
  print *, "Test completed!"
  
end program test_glsl_compute