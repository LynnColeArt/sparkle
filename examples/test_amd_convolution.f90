program test_amd_convolution
  use iso_c_binding
  use iso_fortran_env
  use sparkle_types
  use sparkle_gpu_opengl
  use sparkle_glsl_generator
  use sparkle_glsl_compute
  implicit none
  
  type(gpu_context) :: ctx
  type(glsl_compute_shader) :: conv_shader
  type(glsl_buffer) :: input_buffer, weight_buffer, output_buffer
  integer :: status, i, j
  
  ! Small convolution example
  integer :: batch_size = 1
  integer :: in_channels = 3
  integer :: out_channels = 16
  integer :: height = 32
  integer :: width = 32
  integer :: kernel_size = 3
  
  real, allocatable :: input_data(:,:,:,:)    ! NCHW
  real, allocatable :: weight_data(:,:,:,:)   ! OCHW (for kernel_size x kernel_size)
  real, allocatable :: output_data(:,:,:,:)   ! NCHW
  real, allocatable :: expected(:,:,:,:)
  
  integer :: out_height, out_width
  real :: error_sum
  
  print *, "=== Testing AMD GPU Convolution via GLSL ==="
  print *, ""
  
  ! Initialize OpenGL context
  ctx = create_gpu_context()
  if (.not. ctx%is_initialized) then
    print *, "Failed to create OpenGL context"
    stop 1
  end if
  
  print *, "OpenGL context created successfully"
  print *, "  Vendor:", trim(ctx%vendor)
  print *, "  Renderer:", trim(ctx%renderer)
  print *, "  Version:", trim(ctx%gl_version)
  print *, ""
  
  ! Calculate output dimensions (no padding, stride=1)
  out_height = height - kernel_size + 1
  out_width = width - kernel_size + 1
  
  ! Allocate data
  allocate(input_data(batch_size, in_channels, height, width))
  allocate(weight_data(out_channels, in_channels, kernel_size, kernel_size))
  allocate(output_data(batch_size, out_channels, out_height, out_width))
  allocate(expected(batch_size, out_channels, out_height, out_width))
  
  ! Initialize input with simple pattern
  do i = 1, height
    do j = 1, width
      input_data(1, 1, i, j) = real(i + j) * 0.01  ! R channel
      input_data(1, 2, i, j) = real(i - j) * 0.01  ! G channel  
      input_data(1, 3, i, j) = real(i * j) * 0.001 ! B channel
    end do
  end do
  
  ! Initialize weights with simple filters
  do i = 1, out_channels
    do j = 1, in_channels
      ! Simple edge detection kernels, blur kernels, etc
      weight_data(i, j, :, :) = real(i + j) * 0.1
      ! Make center weight stronger
      weight_data(i, j, 2, 2) = real(i + j) * 0.3
    end do
  end do
  
  print *, "Input shape: ", shape(input_data)
  print *, "Weight shape:", shape(weight_data)
  print *, "Output shape:", shape(output_data)
  print *, ""
  
  ! Create convolution configuration
  block
    type(glsl_kernel_config) :: config
    character(len=:), allocatable :: shader_source
    
    config%M = out_channels                           ! Output channels
    config%N = out_height * out_width                 ! Output spatial size
    config%K = in_channels * kernel_size * kernel_size ! Input patch size
    config%tile_m = 4
    config%tile_n = 64
    config%tile_k = 4
    
    print *, "Convolution as GEMM dimensions:"
    print *, "  M (output channels):", config%M
    print *, "  N (output locations):", config%N  
    print *, "  K (input patch size):", config%K
    print *, ""
    
    ! Generate GLSL compute shader for convolution
    shader_source = generate_conv_glsl_shader(config)
    
    ! Create and compile shader
    conv_shader = create_glsl_compute_shader(ctx, shader_source)
    if (conv_shader%shader_id == 0) then
      print *, "Failed to create compute shader"
      stop 1
    end if
    
    print *, "✅ Compiled GLSL compute shader for convolution"
    
    ! Create GPU buffers
    input_buffer = create_glsl_buffer(ctx, &
      batch_size * in_channels * height * width * 4_c_size_t)
    weight_buffer = create_glsl_buffer(ctx, &
      out_channels * in_channels * kernel_size * kernel_size * 4_c_size_t)  
    output_buffer = create_glsl_buffer(ctx, &
      batch_size * out_channels * out_height * out_width * 4_c_size_t)
    
    if (input_buffer%buffer_id == 0 .or. &
        weight_buffer%buffer_id == 0 .or. &
        output_buffer%buffer_id == 0) then
      print *, "Failed to create GPU buffers"
      stop 1
    end if
    
    print *, "✅ Created GPU buffers"
    
    ! Upload data to GPU
    call upload_buffer_data(input_buffer, c_loc(input_data), &
      batch_size * in_channels * height * width * 4_c_size_t)
    call upload_buffer_data(weight_buffer, c_loc(weight_data), &
      out_channels * in_channels * kernel_size * kernel_size * 4_c_size_t)
    
    print *, "✅ Uploaded data to GPU"
    
    ! Bind buffers to shader
    call bind_compute_buffer(conv_shader, input_buffer, 0)   ! Input im2col
    call bind_compute_buffer(conv_shader, weight_buffer, 1)  ! Weights
    call bind_compute_buffer(conv_shader, output_buffer, 2)  ! Output
    
    ! Set uniforms for convolution parameters
    call glUseProgram(conv_shader%shader_id)
    call set_uniform_int(conv_shader, "in_height", height)
    call set_uniform_int(conv_shader, "in_width", width)
    call set_uniform_int(conv_shader, "kernel_size", kernel_size)
    call set_uniform_int(conv_shader, "in_channels", in_channels)
    call set_uniform_int(conv_shader, "out_height", out_height)
    call set_uniform_int(conv_shader, "out_width", out_width)
    
    ! Dispatch compute shader
    print *, ""
    print *, "Dispatching compute shader..."
    call dispatch_glsl_compute(conv_shader, &
      (config%M + config%tile_m - 1) / config%tile_m, &
      (config%N + config%tile_n - 1) / config%tile_n, &
      1)
    
    ! Wait for completion
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    print *, "✅ Compute shader completed"
    
    ! Read results back
    call download_buffer_data(output_buffer, c_loc(output_data), &
      batch_size * out_channels * out_height * out_width * 4_c_size_t)
    
    print *, "✅ Downloaded results from GPU"
  end block
  
  ! Compute expected results on CPU for verification
  print *, ""
  print *, "Computing expected results on CPU..."
  call compute_conv_reference(input_data, weight_data, expected, kernel_size)
  
  ! Verify results
  print *, ""
  print *, "Verification:"
  error_sum = 0.0
  do i = 1, out_channels
    do j = 1, min(3, out_height)
      print '(A,I2,A,I2,A,F8.4,A,F8.4)', &
        "  Output[1,", i, ",", j, ",1] = ", output_data(1,i,j,1), &
        " (expected: ", expected(1,i,j,1), ")"
      error_sum = error_sum + abs(output_data(1,i,j,1) - expected(1,i,j,1))
    end do
  end do
  
  print *, ""
  print *, "Average error:", error_sum / (out_channels * out_height * out_width)
  
  if (error_sum / (out_channels * out_height * out_width) < 0.001) then
    print *, ""
    print *, "✅ Convolution results verified! GPU computation working!"
  else
    print *, "❌ Results don't match expected values"
  end if
  
  ! Cleanup
  call destroy_glsl_buffer(input_buffer)
  call destroy_glsl_buffer(weight_buffer)
  call destroy_glsl_buffer(output_buffer)
  call destroy_glsl_compute_shader(conv_shader)
  call destroy_gpu_context(ctx)
  
  deallocate(input_data, weight_data, output_data, expected)
  
contains

  subroutine compute_conv_reference(input, weights, output, ksize)
    real, intent(in) :: input(:,:,:,:)    ! NCHW
    real, intent(in) :: weights(:,:,:,:)  ! OCHW
    real, intent(out) :: output(:,:,:,:)  ! NCHW
    integer, intent(in) :: ksize
    
    integer :: b, oc, oh, ow, ic, kh, kw
    integer :: ih, iw
    real :: sum
    
    output = 0.0
    
    do b = 1, size(output, 1)
      do oc = 1, size(output, 2)
        do oh = 1, size(output, 3)
          do ow = 1, size(output, 4)
            sum = 0.0
            do ic = 1, size(input, 2)
              do kh = 1, ksize
                do kw = 1, ksize
                  ih = oh + kh - 1
                  iw = ow + kw - 1
                  sum = sum + input(b, ic, ih, iw) * weights(oc, ic, kh, kw)
                end do
              end do
            end do
            output(b, oc, oh, ow) = sum
          end do
        end do
      end do
    end do
  end subroutine compute_conv_reference

end program test_amd_convolution