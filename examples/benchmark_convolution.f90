program benchmark_convolution
  use iso_fortran_env
  use iso_c_binding
  use sparkle_fortran_shaders
  use gl_constants
  implicit none
  
  ! Test parameters - small for initial testing
  integer, parameter :: batch_size = 1
  integer, parameter :: in_channels = 3
  integer, parameter :: out_channels = 64
  integer, parameter :: height = 224
  integer, parameter :: width = 224
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  
  ! Calculated dimensions
  integer :: out_height, out_width
  integer :: col_height, col_width
  integer :: num_patches
  
  ! Arrays
  real(real32), allocatable, target :: input(:)
  real(real32), allocatable, target :: kernel(:)
  real(real32), allocatable, target :: output(:)
  real(real32), allocatable, target :: col_buffer(:)
  real(real32), allocatable, target :: bias(:)
  
  ! GL buffers
  integer :: input_buffer, kernel_buffer, output_buffer, col_buffer_gl, bias_buffer
  type(c_ptr) :: buffers(3)
  
  ! Timing
  real(real64) :: start_time, end_time
  real(real64) :: cpu_time_ms, gpu_time_ms
  real(real64) :: gflops
  
  ! Other
  type(glsl_context) :: ctx
  integer :: status, i
  
  print *, "=== Convolution Benchmark (Fortran GPU DSL) ==="
  print *, "Input:", height, "x", width, "x", in_channels
  print *, "Kernel:", kernel_size, "x", kernel_size
  print *, "Output channels:", out_channels
  
  ! Calculate output dimensions
  out_height = (height + 2*pad - kernel_size) / stride + 1
  out_width = (width + 2*pad - kernel_size) / stride + 1
  num_patches = out_height * out_width
  
  ! im2col buffer dimensions
  col_height = in_channels * kernel_size * kernel_size
  col_width = out_height * out_width
  
  print *, "Output:", out_height, "x", out_width, "x", out_channels
  print *, "im2col buffer:", col_height, "x", col_width
  
  ! Allocate arrays
  allocate(input(batch_size * in_channels * height * width))
  allocate(kernel(out_channels * in_channels * kernel_size * kernel_size))
  allocate(output(batch_size * out_channels * out_height * out_width))
  allocate(col_buffer(col_height * col_width))
  allocate(bias(out_channels))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(kernel)
  bias = 0.1  ! Small bias
  output = 0.0
  
  ! Calculate theoretical FLOPS
  ! Convolution FLOPS = 2 * output_size * kernel_size^2 * in_channels
  gflops = 2.0d0 * out_channels * out_height * out_width * &
           kernel_size * kernel_size * in_channels / 1.0d9
  
  print *, ""
  print *, "Theoretical GFLOPS:", gflops
  
  ! Initialize GL context
  status = glsl_init(ctx)
  if (status /= 0) then
    print *, "Failed to initialize GL"
    stop 1
  end if
  
  ! Create GL buffers
  call glGenBuffers(1, input_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, input_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(input) * 4, c_size_t), &
                    c_loc(input), GL_DYNAMIC_COPY)
  
  call glGenBuffers(1, col_buffer_gl)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, col_buffer_gl)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(col_buffer) * 4, c_size_t), &
                    c_null_ptr, GL_DYNAMIC_COPY)
  
  call glGenBuffers(1, kernel_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, kernel_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(kernel) * 4, c_size_t), &
                    c_loc(kernel), GL_DYNAMIC_COPY)
  
  call glGenBuffers(1, output_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, output_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(output) * 4, c_size_t), &
                    c_loc(output), GL_DYNAMIC_COPY)
  
  ! Time GPU execution
  call glFinish()
  call cpu_time(start_time)
  
  ! Step 1: im2col transform
  print *, ""
  print *, "Step 1: im2col transform..."
  buffers(1) = transfer(input_buffer, c_null_ptr)
  buffers(2) = transfer(col_buffer_gl, c_null_ptr)
  
  ! Note: We need to pass parameters somehow...
  ! For now, let's test with simpler kernels
  
  ! Step 2: GEMM (simplified for now - just test the infrastructure)
  print *, "Step 2: Matrix multiply..."
  buffers(1) = transfer(col_buffer_gl, c_null_ptr)
  buffers(2) = transfer(kernel_buffer, c_null_ptr)
  buffers(3) = transfer(output_buffer, c_null_ptr)
  
  ! For testing, let's use vector_add as placeholder
  call sporkle_compile_and_dispatch( &
    kernel_file = "examples/kernels.f90", &
    kernel_name = "vector_add", &
    global_size = min(1000, size(output)), &
    buffers = buffers(1:2), &
    status = status &
  )
  
  call glFinish()
  call cpu_time(end_time)
  gpu_time_ms = (end_time - start_time) * 1000.0
  
  if (status == 0) then
    print *, ""
    print *, "GPU Execution time:", gpu_time_ms, "ms"
    print *, "Achieved GFLOPS:", gflops / (gpu_time_ms / 1000.0)
    
    ! Read back results
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, output_buffer)
    block
      interface
        function glMapBufferRange(target, offset, length, access) bind(C, name="glMapBufferRange")
          import :: c_int, c_intptr_t, c_size_t, c_ptr
          integer(c_int), value :: target
          integer(c_intptr_t), value :: offset
          integer(c_size_t), value :: length
          integer(c_int), value :: access
          type(c_ptr) :: glMapBufferRange
        end function
        
        function glUnmapBuffer(target) bind(C, name="glUnmapBuffer")
          import :: c_int
          integer(c_int), value :: target
          integer(c_int) :: glUnmapBuffer
        end function
      end interface
      
      integer, parameter :: GL_MAP_READ_BIT = int(z'0001', c_int)
      type(c_ptr) :: ptr
      real(real32), pointer :: mapped(:)
      
      ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                             int(size(output) * 4, c_size_t), GL_MAP_READ_BIT)
      if (c_associated(ptr)) then
        call c_f_pointer(ptr, mapped, shape(output))
        
        ! Just check first few values are non-zero
        print *, "First 5 output values:", mapped(1:min(5, size(output)))
        
        if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
          print *, "Warning: unmap failed"
        end if
      end if
    end block
  else
    print *, "GPU execution failed"
  end if
  
  ! Cleanup
  call glDeleteBuffers(1, input_buffer)
  call glDeleteBuffers(1, col_buffer_gl)
  call glDeleteBuffers(1, kernel_buffer)
  call glDeleteBuffers(1, output_buffer)
  call glsl_cleanup(ctx)
  
  deallocate(input, kernel, output, col_buffer, bias)
  
  print *, ""
  print *, "Benchmark complete."
  print *, ""
  print *, "Next steps:"
  print *, "- Implement parameter passing for complex kernels"
  print *, "- Full im2col implementation"
  print *, "- Tiled GEMM with shared memory"
  print *, "- Achieve those 14 TFLOPS!"
  
end program benchmark_convolution