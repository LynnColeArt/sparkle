module sparkle_glsl_compute
  use iso_c_binding
  use sparkle_types
  use sparkle_glsl_generator
  use sparkle_amdgpu_direct  ! For buffer types
  implicit none
  
  private
  public :: glsl_kernel, compile_glsl_kernel, dispatch_conv_glsl, cleanup_glsl_kernel
  public :: init_opengl_compute, cleanup_opengl_compute
  
  ! OpenGL constants
  integer(c_int), parameter :: GL_COMPUTE_SHADER = int(z'91B9', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BARRIER_BIT = int(z'2000', c_int)
  integer(c_int), parameter :: GL_COMPILE_STATUS = int(z'8B81', c_int)
  integer(c_int), parameter :: GL_LINK_STATUS = int(z'8B82', c_int)
  integer(c_int), parameter :: GL_INFO_LOG_LENGTH = int(z'8B84', c_int)
  
  ! GLSL kernel structure
  type :: glsl_kernel
    integer(c_int) :: shader_id
    integer(c_int) :: program_id
    type(convolution_config) :: config
    logical :: is_valid
  end type glsl_kernel
  
  ! OpenGL function interfaces
  interface
    function glCreateShader(shader_type) bind(C, name="glCreateShader")
      import :: c_int
      integer(c_int), value :: shader_type
      integer(c_int) :: glCreateShader
    end function
    
    subroutine glShaderSource(shader, count, string, length) bind(C, name="glShaderSource")
      import :: c_int, c_ptr
      integer(c_int), value :: shader
      integer(c_int), value :: count
      type(c_ptr), intent(in) :: string
      type(c_ptr), value :: length
    end subroutine
    
    subroutine glCompileShader(shader) bind(C, name="glCompileShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glGetShaderiv(shader, pname, params) bind(C, name="glGetShaderiv")
      import :: c_int
      integer(c_int), value :: shader
      integer(c_int), value :: pname
      integer(c_int), intent(out) :: params
    end subroutine
    
    subroutine glGetShaderInfoLog(shader, buf_size, length, info_log) bind(C, name="glGetShaderInfoLog")
      import :: c_int, c_char
      integer(c_int), value :: shader
      integer(c_int), value :: buf_size
      integer(c_int), intent(out) :: length
      character(c_char), intent(out) :: info_log(*)
    end subroutine
    
    function glCreateProgram() bind(C, name="glCreateProgram")
      import :: c_int
      integer(c_int) :: glCreateProgram
    end function
    
    subroutine glAttachShader(program, shader) bind(C, name="glAttachShader")
      import :: c_int
      integer(c_int), value :: program
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glLinkProgram(program) bind(C, name="glLinkProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glGetProgramiv(program, pname, params) bind(C, name="glGetProgramiv")
      import :: c_int
      integer(c_int), value :: program
      integer(c_int), value :: pname
      integer(c_int), intent(out) :: params
    end subroutine
    
    subroutine glGetProgramInfoLog(program, buf_size, length, info_log) bind(C, name="glGetProgramInfoLog")
      import :: c_int, c_char
      integer(c_int), value :: program
      integer(c_int), value :: buf_size
      integer(c_int), intent(out) :: length
      character(c_char), intent(out) :: info_log(*)
    end subroutine
    
    subroutine glUseProgram(program) bind(C, name="glUseProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    function glGetUniformLocation(program, name) bind(C, name="glGetUniformLocation")
      import :: c_int, c_char
      integer(c_int), value :: program
      character(c_char), intent(in) :: name(*)
      integer(c_int) :: glGetUniformLocation
    end function
    
    subroutine glUniform1i(location, v0) bind(C, name="glUniform1i")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: v0
    end subroutine
    
    subroutine glBindBufferBase(target, index, buffer) bind(C, name="glBindBufferBase")
      import :: c_int
      integer(c_int), value :: target
      integer(c_int), value :: index
      integer(c_int), value :: buffer
    end subroutine
    
    subroutine glDispatchCompute(num_groups_x, num_groups_y, num_groups_z) bind(C, name="glDispatchCompute")
      import :: c_int
      integer(c_int), value :: num_groups_x
      integer(c_int), value :: num_groups_y
      integer(c_int), value :: num_groups_z
    end subroutine
    
    subroutine glMemoryBarrier(barriers) bind(C, name="glMemoryBarrier")
      import :: c_int
      integer(c_int), value :: barriers
    end subroutine
    
    subroutine glDeleteShader(shader) bind(C, name="glDeleteShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glDeleteProgram(program) bind(C, name="glDeleteProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
  end interface
  
contains

  function init_opengl_compute() result(status)
    integer :: status
    
    ! In a real implementation, we would:
    ! 1. Create an OpenGL context (via EGL or GLX)
    ! 2. Make it current
    ! 3. Initialize GL extensions
    
    ! For now, assume OpenGL context exists
    status = 0
  end function init_opengl_compute
  
  subroutine cleanup_opengl_compute()
    ! Clean up OpenGL context
  end subroutine cleanup_opengl_compute

  function compile_glsl_kernel(config) result(kernel)
    type(convolution_config), intent(in) :: config
    type(glsl_kernel) :: kernel
    character(len=:), allocatable :: shader_source
    character(len=1, kind=c_char), allocatable, target :: c_source(:)
    type(c_ptr) :: source_ptr
    integer(c_int) :: compile_status, link_status
    integer(c_int) :: log_length
    character(len=1024) :: error_log
    integer :: i
    
    kernel%config = config
    kernel%is_valid = .false.
    
    ! Generate shader source
    shader_source = generate_conv_glsl_shader(config)
    
    ! Convert to C string
    allocate(c_source(len(shader_source) + 1))
    do i = 1, len(shader_source)
      c_source(i) = shader_source(i:i)
    end do
    c_source(len(shader_source) + 1) = c_null_char
    source_ptr = c_loc(c_source)
    
    ! Create and compile shader
    kernel%shader_id = glCreateShader(GL_COMPUTE_SHADER)
    if (kernel%shader_id == 0) then
      print *, "ERROR: Failed to create shader"
      return
    end if
    
    call glShaderSource(kernel%shader_id, 1, source_ptr, c_null_ptr)
    call glCompileShader(kernel%shader_id)
    
    ! Check compilation
    call glGetShaderiv(kernel%shader_id, GL_COMPILE_STATUS, compile_status)
    if (compile_status == 0) then
      call glGetShaderiv(kernel%shader_id, GL_INFO_LOG_LENGTH, log_length)
      if (log_length > 0) then
        call glGetShaderInfoLog(kernel%shader_id, min(log_length, 1024), log_length, error_log)
        print *, "Shader compilation failed:"
        print *, error_log(1:log_length)
      end if
      call glDeleteShader(kernel%shader_id)
      return
    end if
    
    ! Create and link program
    kernel%program_id = glCreateProgram()
    if (kernel%program_id == 0) then
      print *, "ERROR: Failed to create program"
      call glDeleteShader(kernel%shader_id)
      return
    end if
    
    call glAttachShader(kernel%program_id, kernel%shader_id)
    call glLinkProgram(kernel%program_id)
    
    ! Check linking
    call glGetProgramiv(kernel%program_id, GL_LINK_STATUS, link_status)
    if (link_status == 0) then
      call glGetProgramiv(kernel%program_id, GL_INFO_LOG_LENGTH, log_length)
      if (log_length > 0) then
        call glGetProgramInfoLog(kernel%program_id, min(log_length, 1024), log_length, error_log)
        print *, "Program linking failed:"
        print *, error_log(1:log_length)
      end if
      call glDeleteProgram(kernel%program_id)
      call glDeleteShader(kernel%shader_id)
      return
    end if
    
    kernel%is_valid = .true.
    
    ! Clean up
    deallocate(c_source)
    
  end function compile_glsl_kernel
  
  subroutine dispatch_conv_glsl(kernel, input_buffer, kernel_buffer, output_buffer, M, N, K)
    type(glsl_kernel), intent(in) :: kernel
    type(amdgpu_buffer), intent(in) :: input_buffer, kernel_buffer
    type(amdgpu_buffer), intent(inout) :: output_buffer
    integer, intent(in) :: M, N, K
    integer(c_int) :: loc_M, loc_N, loc_K
    integer(c_int) :: groups_x, groups_y, groups_z
    character(len=2, kind=c_char) :: c_M, c_N, c_K
    
    if (.not. kernel%is_valid) then
      print *, "ERROR: Invalid kernel"
      return
    end if
    
    ! Use program
    call glUseProgram(kernel%program_id)
    
    ! Bind buffers
    ! Note: In real implementation, we'd need GL buffer objects
    ! For now, assuming buffers have GL IDs
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, int(input_buffer%handle, c_int))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, int(kernel_buffer%handle, c_int))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, int(output_buffer%handle, c_int))
    
    ! Set uniforms
    c_M = "M" // c_null_char
    c_N = "N" // c_null_char
    c_K = "K" // c_null_char
    
    loc_M = glGetUniformLocation(kernel%program_id, c_M)
    loc_N = glGetUniformLocation(kernel%program_id, c_N)
    loc_K = glGetUniformLocation(kernel%program_id, c_K)
    
    if (loc_M >= 0) call glUniform1i(loc_M, M)
    if (loc_N >= 0) call glUniform1i(loc_N, N)
    if (loc_K >= 0) call glUniform1i(loc_K, K)
    
    ! Calculate dispatch dimensions
    groups_x = (N + 15) / 16  ! 16 = local_size_x
    groups_y = (M + 15) / 16  ! 16 = local_size_y
    groups_z = 1
    
    ! Dispatch compute
    call glDispatchCompute(groups_x, groups_y, groups_z)
    
    ! Ensure completion
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    
  end subroutine dispatch_conv_glsl
  
  subroutine cleanup_glsl_kernel(kernel)
    type(glsl_kernel), intent(inout) :: kernel
    
    if (kernel%program_id /= 0) then
      call glDeleteProgram(kernel%program_id)
      kernel%program_id = 0
    end if
    
    if (kernel%shader_id /= 0) then
      call glDeleteShader(kernel%shader_id)
      kernel%shader_id = 0
    end if
    
    kernel%is_valid = .false.
  end subroutine cleanup_glsl_kernel

end module sparkle_glsl_compute