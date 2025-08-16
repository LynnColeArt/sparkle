module gl_constants
  use iso_c_binding
  implicit none
  
  ! OpenGL constants
  integer(c_int), parameter :: GL_COMPUTE_SHADER = int(z'91B9', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BARRIER_BIT = int(z'2000', c_int)
  integer(c_int), parameter :: GL_COMPILE_STATUS = int(z'8B81', c_int)
  integer(c_int), parameter :: GL_LINK_STATUS = int(z'8B82', c_int)
  integer(c_int), parameter :: GL_INFO_LOG_LENGTH = int(z'8B84', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_COPY = int(z'88EA', c_int)
  
  ! Additional constants for GLSL compute
  integer(c_int), parameter :: GL_FALSE = 0
  integer(c_int), parameter :: GL_TRUE = 1
  
  ! OpenGL function interfaces that are commonly used
  interface
    function glCreateShader(shader_type) bind(C, name="glCreateShader")
      import :: c_int
      integer(c_int), value :: shader_type
      integer(c_int) :: glCreateShader
    end function
    
    function glCreateProgram() bind(C, name="glCreateProgram")
      import :: c_int
      integer(c_int) :: glCreateProgram
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
    
    subroutine glGetShaderInfoLog(shader, bufSize, length, infoLog) bind(C, name="glGetShaderInfoLog")
      import :: c_int, c_ptr
      integer(c_int), value :: shader
      integer(c_int), value :: bufSize
      type(c_ptr), value :: length
      type(c_ptr), value :: infoLog
    end subroutine
    
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
    
    subroutine glGetProgramInfoLog(program, bufSize, length, infoLog) bind(C, name="glGetProgramInfoLog")
      import :: c_int, c_ptr
      integer(c_int), value :: program
      integer(c_int), value :: bufSize
      type(c_ptr), value :: length
      type(c_ptr), value :: infoLog
    end subroutine
    
    subroutine glUseProgram(program) bind(C, name="glUseProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glDeleteShader(shader) bind(C, name="glDeleteShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glDeleteProgram(program) bind(C, name="glDeleteProgram")
      import :: c_int
      integer(c_int), value :: program
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
    
    subroutine glFinish() bind(C, name="glFinish")
    end subroutine
    
    subroutine glGenBuffers(n, buffers) bind(C, name="glGenBuffers")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(out) :: buffers
    end subroutine
    
    subroutine glBindBuffer(target, buffer) bind(C, name="glBindBuffer")
      import :: c_int
      integer(c_int), value :: target
      integer(c_int), value :: buffer
    end subroutine
    
    subroutine glBufferData(target, size, data, usage) bind(C, name="glBufferData")
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
      integer(c_int), value :: usage
    end subroutine
    
    subroutine glBindBufferBase(target, index, buffer) bind(C, name="glBindBufferBase")
      import :: c_int
      integer(c_int), value :: target
      integer(c_int), value :: index
      integer(c_int), value :: buffer
    end subroutine
    
    subroutine glGetBufferSubData(target, offset, size, data) bind(C, name="glGetBufferSubData")
      import :: c_int, c_intptr_t, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_intptr_t), value :: offset
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
    end subroutine
    
    subroutine glDeleteBuffers(n, buffers) bind(C, name="glDeleteBuffers")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(in) :: buffers
    end subroutine
    
    ! ========================================================================
    ! Uniform variable functions - Added for adaptive parameter passing
    ! ========================================================================
    
    ! glGetUniformLocation - Get the location of a uniform variable
    ! Returns -1 if the uniform doesn't exist or is not active
    function glGetUniformLocation(program, name) bind(C, name="glGetUniformLocation")
      import :: c_int, c_char
      integer(c_int), value :: program
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int) :: glGetUniformLocation
    end function
    
    ! glUniform1i - Set a single integer uniform
    subroutine glUniform1i(location, v0) bind(C, name="glUniform1i")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: v0
    end subroutine
    
    ! glUniform1f - Set a single float uniform
    subroutine glUniform1f(location, v0) bind(C, name="glUniform1f")
      import :: c_int, c_float
      integer(c_int), value :: location
      real(c_float), value :: v0
    end subroutine
    
    ! glUniform2i - Set a 2-component integer uniform
    subroutine glUniform2i(location, v0, v1) bind(C, name="glUniform2i")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: v0, v1
    end subroutine
    
    ! glUniform3i - Set a 3-component integer uniform
    subroutine glUniform3i(location, v0, v1, v2) bind(C, name="glUniform3i")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: v0, v1, v2
    end subroutine
    
    ! glUniform4i - Set a 4-component integer uniform
    subroutine glUniform4i(location, v0, v1, v2, v3) bind(C, name="glUniform4i")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: v0, v1, v2, v3
    end subroutine
    
    ! glUniform1iv - Set an array of integer uniforms
    subroutine glUniform1iv(location, count, value) bind(C, name="glUniform1iv")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: count
      integer(c_int), intent(in) :: value(*)
    end subroutine
    
    ! glUniform4iv - Set an array of 4-component integer uniforms
    subroutine glUniform4iv(location, count, value) bind(C, name="glUniform4iv")
      import :: c_int
      integer(c_int), value :: location
      integer(c_int), value :: count
      integer(c_int), intent(in) :: value(*)
    end subroutine
    
    ! glBufferSubData - Update a subset of a buffer object's data store
    ! This is essential for updating parameter buffers without reallocating
    subroutine glBufferSubData(target, offset, size, data) bind(C, name="glBufferSubData")
      import :: c_int, c_intptr_t, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_intptr_t), value :: offset
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
    end subroutine
  end interface
  
end module gl_constants