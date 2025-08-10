program test_compute_shader
  use iso_fortran_env, only: int32, int64, real32, real64
  use sparkle_types
  use sparkle_memory
  use sparkle_kernels
  use sparkle_compute_shader
  implicit none
  
  type(sparkle_kernel) :: kernel
  type(compute_shader) :: shader
  type(memory_handle) :: a_mem, b_mem, c_mem
  integer(int64) :: n
  
  print *, "🎮 Testing Compute Shader Translation"
  print *, "===================================="
  print *, ""
  
  ! Create a simple vector addition kernel
  n = 1024
  
  ! Build kernel
  kernel%name = 'vector_add'
  kernel%kernel_type = KERNEL_PURE
  kernel%work_items = n
  kernel%block_size = 64
  
  allocate(kernel%arguments(3))
  
  ! Input A
  kernel%arguments(1)%name = 'a'
  kernel%arguments(1)%arg_type = TYPE_REAL32
  kernel%arguments(1)%intent = ARG_IN
  kernel%arguments(1)%rank = 1
  
  ! Input B
  kernel%arguments(2)%name = 'b'
  kernel%arguments(2)%arg_type = TYPE_REAL32
  kernel%arguments(2)%intent = ARG_IN
  kernel%arguments(2)%rank = 1
  
  ! Output C
  kernel%arguments(3)%name = 'c'
  kernel%arguments(3)%arg_type = TYPE_REAL32
  kernel%arguments(3)%intent = ARG_OUT
  kernel%arguments(3)%rank = 1
  
  print *, "Kernel Configuration:"
  print '(A,A)', "  Name: ", kernel%name
  print '(A,I0)', "  Work items: ", kernel%work_items
  print '(A,I0)', "  Block size: ", kernel%block_size
  print *, ""
  
  ! Compile to compute shader
  shader = compile_fortran_to_glsl(kernel)
  
  ! Test dispatch
  print *, ""
  print *, "Testing shader dispatch..."
  call shader%dispatch([n, 1_int64, 1_int64])
  
  ! Now test a reduction kernel
  print *, ""
  print *, "Testing reduction kernel translation..."
  print *, "======================================"
  
  kernel%name = 'sum_reduction'
  kernel%kernel_type = KERNEL_REDUCTION
  
  deallocate(kernel%arguments)
  allocate(kernel%arguments(2))
  
  kernel%arguments(1)%name = 'input'
  kernel%arguments(1)%arg_type = TYPE_REAL32
  kernel%arguments(1)%intent = ARG_IN
  kernel%arguments(1)%rank = 1
  
  kernel%arguments(2)%name = 'sum'
  kernel%arguments(2)%arg_type = TYPE_REAL32
  kernel%arguments(2)%intent = ARG_OUT
  kernel%arguments(2)%rank = 0
  
  shader = compile_fortran_to_glsl(kernel)
  
  ! Test custom kernel with double precision
  print *, ""
  print *, "Testing custom kernel with double precision..."
  print *, "==========================================="
  
  kernel%name = 'custom_compute'
  kernel%kernel_type = KERNEL_PURE
  kernel%arguments(1)%arg_type = TYPE_REAL64
  kernel%arguments(2)%arg_type = TYPE_REAL64
  
  shader = compile_fortran_to_glsl(kernel)
  
  print *, ""
  print *, "✅ Compute shader translation test complete!"
  print *, ""
  print *, "Note: This demonstrates GLSL generation from Fortran kernels."
  print *, "Actual GPU execution would require OpenGL/Vulkan context."
  print *, ""
  print *, "The Sparkle Way: Write Fortran, run everywhere! 🌟"

end program test_compute_shader