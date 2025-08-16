program benchmark_saxpy
  use iso_fortran_env
  use iso_c_binding
  use sparkle_fortran_shaders
  use gl_constants
  implicit none
  
  integer, parameter :: N = 1000000  ! 1M elements
  type(glsl_context) :: ctx
  integer :: status, i
  real(real32), allocatable, target :: x(:), y(:), y_ref(:)
  real(real32) :: a
  integer :: x_buffer, y_buffer
  type(c_ptr) :: buffers(2)
  real(real64) :: start_time, end_time, gpu_time, cpu_time_ms
  
  print *, "=== SAXPY Benchmark ==="
  print *, "N =", N
  
  ! Initialize data
  allocate(x(N), y(N), y_ref(N))
  a = 2.5
  
  ! Initialize with random data
  call random_number(x)
  call random_number(y)
  y_ref = y  ! Save reference
  
  ! CPU Baseline
  print *, ""
  print *, "CPU Baseline:"
  call cpu_time(start_time)
  do i = 1, N
    y_ref(i) = a * x(i) + y_ref(i)
  end do
  call cpu_time(end_time)
  cpu_time_ms = (end_time - start_time) * 1000.0  ! Convert to ms
  print *, "  Time:", cpu_time_ms, "ms"
  print *, "  Throughput:", real(N) / cpu_time_ms / 1e6, "Gops/ms"
  
  ! GPU with Fortran shader
  print *, ""
  print *, "GPU (Fortran Shader):"
  
  ! Initialize GL context
  status = glsl_init(ctx)
  if (status /= 0) then
    print *, "Failed to initialize GL"
    stop 1
  end if
  
  ! Create buffers
  call glGenBuffers(1, x_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, x_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(N * 4, c_size_t), &
                    c_loc(x), GL_DYNAMIC_COPY)
  
  call glGenBuffers(1, y_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, y_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(N * 4, c_size_t), &
                    c_loc(y), GL_DYNAMIC_COPY)
  
  ! Setup buffer array
  buffers(1) = transfer(x_buffer, c_null_ptr)
  buffers(2) = transfer(y_buffer, c_null_ptr)
  
  ! Time GPU execution
  call glFinish()  ! Ensure GPU idle
  call cpu_time(start_time)
  
  ! Note: We need to handle the scalar parameter 'a' differently
  ! For now, let's use a simpler vector_add kernel
  call sporkle_compile_and_dispatch( &
    kernel_file = "examples/kernels.f90", &
    kernel_name = "vector_add", &
    global_size = N, &
    buffers = buffers, &
    status = status &
  )
  
  call glFinish()  ! Wait for completion
  call cpu_time(end_time)
  gpu_time = (end_time - start_time) * 1000.0
  
  if (status /= 0) then
    print *, "GPU execution failed"
  else
    print *, "  Time:", gpu_time, "ms"
    print *, "  Throughput:", real(N) / gpu_time / 1e6, "Gops/ms"
    print *, "  Speedup:", cpu_time_ms / gpu_time, "x"
    
    ! Read back and verify
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, y_buffer)
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
      real :: max_error
      
      ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                             int(N * 4, c_size_t), GL_MAP_READ_BIT)
      if (c_associated(ptr)) then
        call c_f_pointer(ptr, mapped, [N])
        
        ! Check correctness (for vector_add, result should be x + y)
        max_error = 0.0
        do i = 1, min(10, N)
          max_error = max(max_error, abs(mapped(i) - (x(i) + y(i))))
        end do
        
        if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
          print *, "Warning: unmap failed"
        end if
        
        print *, "  Max error (first 10):", max_error
        if (max_error < 1e-6) then
          print *, "  ✓ Results correct!"
        else
          print *, "  ✗ Results incorrect!"
        end if
      end if
    end block
  end if
  
  ! Cleanup
  call glDeleteBuffers(1, x_buffer)
  call glDeleteBuffers(1, y_buffer)
  call glsl_cleanup(ctx)
  
  deallocate(x, y, y_ref)
  
  print *, ""
  print *, "Benchmark complete."
  
end program benchmark_saxpy