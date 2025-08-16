! Fortran interface to GPU OpenGL reference implementation
! This provides a clean Fortran API to the working C implementation

module gpu_opengl_interface
  use iso_fortran_env, only: real32
  use iso_c_binding
  implicit none
  
  private
  public :: gpu_conv2d_params, gpu_init, gpu_cleanup
  public :: gpu_compile_shaders, gpu_execute_conv2d_ref, gpu_get_program_id
  
  ! Parameter structure matching C implementation
  type, bind(C) :: gpu_conv2d_params
    integer(c_int) :: N, H, W, C, K
    integer(c_int) :: kernel_size, stride, pad
    integer(c_int) :: H_out, W_out
  end type gpu_conv2d_params
  
  ! C function interfaces
  interface
    ! Initialize GPU context
    function gpu_initialize_opengl() bind(C, name="gpu_initialize_opengl")
      import :: c_int
      integer(c_int) :: gpu_initialize_opengl
    end function
    
    ! Cleanup GPU context
    subroutine gpu_cleanup_opengl() bind(C, name="gpu_cleanup_opengl")
    end subroutine
    
    ! Check if GPU is initialized
    function gpu_is_initialized() bind(C, name="gpu_is_initialized")
      import :: c_int
      integer(c_int) :: gpu_is_initialized
    end function
    
    ! Compile conv2d shader
    function gpu_compile_conv2d_shader() bind(C, name="gpu_compile_conv2d_shader")
      import :: c_int
      integer(c_int) :: gpu_compile_conv2d_shader
    end function
    
    ! Execute conv2d (simplified interface)
    function gpu_execute_conv2d_fortran(input, weights, output, &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out) &
             bind(C, name="gpu_execute_conv2d_fortran")
      import :: c_ptr, c_float, c_int
      type(c_ptr), value :: input, weights, output
      integer(c_int), value :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
      real(c_float) :: gpu_execute_conv2d_fortran
    end function
    
    ! Get compute program handle
    function gpu_get_compute_program() bind(C, name="gpu_get_compute_program")
      import :: c_int
      integer(c_int) :: gpu_get_compute_program
    end function
  end interface
  
contains
  
  ! Initialize GPU with error handling
  logical function gpu_init()
    gpu_init = (gpu_initialize_opengl() /= 0)
    if (.not. gpu_init) then
      print *, "Failed to initialize GPU OpenGL context"
      return
    end if
    
    if (gpu_compile_conv2d_shader() == 0) then
      print *, "Failed to compile GPU shaders"
      gpu_init = .false.
      call gpu_cleanup_opengl()
      return
    end if
    
    print *, "GPU OpenGL reference implementation initialized successfully"
  end function gpu_init
  
  ! Cleanup GPU
  subroutine gpu_cleanup()
    call gpu_cleanup_opengl()
  end subroutine gpu_cleanup
  
  ! Wrapper for shader compilation
  logical function gpu_compile_shaders()
    gpu_compile_shaders = (gpu_compile_conv2d_shader() /= 0)
  end function gpu_compile_shaders
  
  ! Execute conv2d with Fortran-friendly interface
  real(real32) function gpu_execute_conv2d_ref(input, weights, output, &
                                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in), target :: input(:), weights(:)
    real(real32), intent(out), target :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(c_float) :: time_ms
    
    ! Execute on GPU using simplified interface
    time_ms = gpu_execute_conv2d_fortran(c_loc(input), c_loc(weights), c_loc(output), &
                                        int(N, c_int), int(C, c_int), int(H, c_int), int(W, c_int), &
                                        int(K, c_int), int(kernel_size, c_int), int(stride, c_int), &
                                        int(pad, c_int), int(H_out, c_int), int(W_out, c_int))
    
    gpu_execute_conv2d_ref = real(time_ms, real32)
  end function gpu_execute_conv2d_ref
  
  ! Get the compute program ID for async execution
  function gpu_get_program_id() result(program_id)
    integer :: program_id
    program_id = gpu_get_compute_program()
  end function gpu_get_program_id
  
end module gpu_opengl_interface