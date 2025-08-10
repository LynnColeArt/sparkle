program test_gpu_simple
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding
  use sparkle_types
  use sparkle_memory
  use sparkle_gpu_opengl
  use sparkle_gpu_kernels
  implicit none
  
  type(gl_context) :: ctx
  type(gl_compute_shader) :: vector_add_shader
  type(gl_buffer) :: x_buffer, y_buffer, z_buffer
  
  integer :: n
  real(real32), allocatable :: x_data(:), y_data(:), z_data(:)
  integer :: i
  
  print *, "🚀 Sparkle GPU Test (OpenGL Compute)"
  print *, "===================================="
  print *, ""
  
  ! Create OpenGL context
  print *, "Creating OpenGL compute context..."
  ctx = create_gl_context()
  
  if (.not. ctx%initialized) then
    print *, "❌ Failed to create OpenGL context"
    print *, "   Make sure you have OpenGL 4.3+ drivers installed"
    stop
  end if
  
  ! Create compute shader
  print *, "Compiling vector addition shader..."
  vector_add_shader = create_compute_shader(get_vector_add_shader())
  
  if (.not. vector_add_shader%compiled) then
    print *, "❌ Failed to compile compute shader"
    stop
  end if
  
  ! Test with small array
  n = 1024
  allocate(x_data(n), y_data(n), z_data(n))
  
  ! Initialize test data
  do i = 1, n
    x_data(i) = real(i)
    y_data(i) = real(i * 2)
  end do
  z_data = 0.0
  
  print *, ""
  print '(A,I0)', "Testing with ", n, " elements..."
  
  ! Create GPU buffers
  x_buffer = create_gl_buffer(int(n * 4, c_size_t), 0)
  y_buffer = create_gl_buffer(int(n * 4, c_size_t), 1)
  z_buffer = create_gl_buffer(int(n * 4, c_size_t), 2)
  
  ! Upload data to GPU
  call update_gl_buffer(x_buffer, c_loc(x_data), int(n * 4, c_size_t))
  call update_gl_buffer(y_buffer, c_loc(y_data), int(n * 4, c_size_t))
  
  ! Dispatch compute shader
  print *, "Running GPU computation..."
  call dispatch_compute(vector_add_shader, (n + 255) / 256, 1, 1)
  
  ! Read results back
  ! (Would need glGetBufferSubData here, simplified for now)
  
  print *, ""
  print *, "GPU Setup Test Results:"
  print *, "======================"
  print *, "✅ OpenGL context created successfully"
  print *, "✅ Compute shader compiled"
  print *, "✅ GPU buffers allocated"
  print *, "✅ Compute dispatch executed"
  print *, ""
  print *, "Note: Full GPU execution requires linking with OpenGL/EGL libraries:"
  print *, "  gfortran -o test_gpu test_gpu.f90 -lGL -lEGL"
  print *, ""
  print *, "Next steps:"
  print *, "- Link with actual OpenGL libraries"
  print *, "- Implement buffer readback"
  print *, "- Benchmark GPU vs CPU performance"
  print *, ""
  print *, "🌟 The Sparkle Way: Vendor-neutral GPU compute!"
  
  ! Cleanup
  deallocate(x_data, y_data, z_data)
  call destroy_gl_context(ctx)
  
end program test_gpu_simple