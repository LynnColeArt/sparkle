program test_glsl_gemm_compute
  use iso_c_binding
  use sparkle_types
  use sparkle_glsl_generator
  implicit none
  
  interface
    ! OpenGL functions we need
    function glCreateShader(shaderType) bind(C, name="glCreateShader")
      import :: c_int
      integer(c_int), value :: shaderType
      integer(c_int) :: glCreateShader
    end function
    
    function glCreateProgram() bind(C, name="glCreateProgram")
      import :: c_int
      integer(c_int) :: glCreateProgram
    end function
    
    subroutine glShaderSource(shader, count, string, length) bind(C, name="glShaderSource")
      import :: c_int, c_ptr
      integer(c_int), value :: shader, count
      type(c_ptr), intent(in) :: string
      type(c_ptr), value :: length
    end subroutine
    
    subroutine glCompileShader(shader) bind(C, name="glCompileShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glAttachShader(program, shader) bind(C, name="glAttachShader")
      import :: c_int
      integer(c_int), value :: program, shader
    end subroutine
    
    subroutine glLinkProgram(program) bind(C, name="glLinkProgram")
      import :: c_int
      integer(c_int), value :: program
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
    
    subroutine glGetShaderiv(shader, pname, params) bind(C, name="glGetShaderiv")
      import :: c_int, c_ptr
      integer(c_int), value :: shader, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetShaderInfoLog(shader, maxLength, length, infoLog) bind(C, name="glGetShaderInfoLog")
      import :: c_int, c_ptr
      integer(c_int), value :: shader, maxLength
      type(c_ptr), value :: length
      type(c_ptr), value :: infoLog
    end subroutine
    
    subroutine glDispatchCompute(x, y, z) bind(C, name="glDispatchCompute")
      import :: c_int
      integer(c_int), value :: x, y, z
    end subroutine
    
    subroutine glMemoryBarrier(barriers) bind(C, name="glMemoryBarrier")
      import :: c_int
      integer(c_int), value :: barriers
    end subroutine
    
    subroutine glFinish() bind(C, name="glFinish")
    end subroutine
    
    function glGenBuffers(n, buffers) bind(C, name="glGenBuffers")
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: buffers
      integer(c_int) :: glGenBuffers
    end function
    
    subroutine glBindBuffer(target, buffer) bind(C, name="glBindBuffer")
      import :: c_int
      integer(c_int), value :: target, buffer
    end subroutine
    
    subroutine glBufferData(target, size, data, usage) bind(C, name="glBufferData")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: target
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
      integer(c_int), value :: usage
    end subroutine
    
    subroutine glBindBufferBase(target, index, buffer) bind(C, name="glBindBufferBase")
      import :: c_int
      integer(c_int), value :: target, index, buffer
    end subroutine
    
    function glMapBuffer(target, access) bind(C, name="glMapBuffer")
      import :: c_int, c_ptr
      integer(c_int), value :: target, access
      type(c_ptr) :: glMapBuffer
    end function
    
    function glUnmapBuffer(target) bind(C, name="glUnmapBuffer")
      import :: c_int
      integer(c_int), value :: target
      integer(c_int) :: glUnmapBuffer
    end function
    
    ! EGL functions for headless context
    function eglGetDisplay(display_id) bind(C, name="eglGetDisplay")
      import :: c_ptr
      type(c_ptr), value :: display_id
      type(c_ptr) :: eglGetDisplay
    end function
    
    function eglInitialize(display, major, minor) bind(C, name="eglInitialize")
      import :: c_ptr, c_int
      type(c_ptr), value :: display
      type(c_ptr), value :: major, minor
      integer(c_int) :: eglInitialize
    end function
    
    function eglBindAPI(api) bind(C, name="eglBindAPI")
      import :: c_int
      integer(c_int), value :: api
      integer(c_int) :: eglBindAPI
    end function
    
    function eglChooseConfig(display, attrib_list, configs, config_size, num_config) &
        bind(C, name="eglChooseConfig")
      import :: c_ptr, c_int
      type(c_ptr), value :: display, attrib_list, configs
      integer(c_int), value :: config_size
      type(c_ptr), value :: num_config
      integer(c_int) :: eglChooseConfig
    end function
    
    function eglCreateContext(display, config, share_context, attrib_list) &
        bind(C, name="eglCreateContext")
      import :: c_ptr
      type(c_ptr), value :: display, config, share_context, attrib_list
      type(c_ptr) :: eglCreateContext
    end function
    
    function eglMakeCurrent(display, draw, read, context) bind(C, name="eglMakeCurrent")
      import :: c_ptr, c_int
      type(c_ptr), value :: display, draw, read, context
      integer(c_int) :: eglMakeCurrent
    end function
  end interface
  
  ! OpenGL constants
  integer(c_int), parameter :: GL_COMPUTE_SHADER = int(z'91B9', c_int)
  integer(c_int), parameter :: GL_COMPILE_STATUS = int(z'8B81', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_COPY = int(z'88EA', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BARRIER_BIT = int(z'2000', c_int)
  integer(c_int), parameter :: GL_READ_WRITE = int(z'88BA', c_int)
  
  ! EGL constants
  integer(c_int), parameter :: EGL_DEFAULT_DISPLAY = 0
  integer(c_int), parameter :: EGL_OPENGL_API = int(z'30A2', c_int)
  integer(c_int), parameter :: EGL_OPENGL_BIT = 8
  integer(c_int), parameter :: EGL_NONE = int(z'3038', c_int)
  integer(c_int), parameter :: EGL_NO_SURFACE = 0
  
  ! Test variables
  type(c_ptr) :: egl_display, egl_context, egl_config
  integer(c_int) :: shader_id, program_id
  integer(c_int) :: buffers(3)
  integer(c_int) :: status, num_configs
  integer(c_int) :: config_attribs(7), context_attribs(5)
  character(len=:), allocatable :: shader_source
  type(c_ptr) :: shader_ptr, mapped_ptr
  character(len=1024) :: info_log
  integer :: i, j, k
  
  ! Matrix dimensions for convolution
  integer :: M = 16    ! Output channels
  integer :: N = 900   ! Output locations (30x30)
  integer :: K_dim = 27    ! Input patch size (3x3x3)
  
  real, allocatable, target :: A(:,:), B(:,:), C(:,:), C_ref(:,:)
  real :: error
  
  print *, "=== Testing GLSL Compute Shader for Convolution ==="
  print *, ""
  
  ! Initialize EGL for headless compute
  print *, "Initializing EGL context..."
  
  egl_display = eglGetDisplay(c_null_ptr)
  if (.not. c_associated(egl_display)) then
    print *, "Failed to get EGL display"
    stop 1
  end if
  
  status = eglInitialize(egl_display, c_null_ptr, c_null_ptr)
  if (status == 0) then
    print *, "Failed to initialize EGL"
    stop 1
  end if
  
  status = eglBindAPI(EGL_OPENGL_API)
  if (status == 0) then
    print *, "Failed to bind OpenGL API"
    stop 1
  end if
  
  ! Choose config
  config_attribs = [EGL_OPENGL_BIT, 1, EGL_NONE, 0, 0, 0, 0]
  status = eglChooseConfig(egl_display, c_loc(config_attribs), &
                          c_loc(egl_config), 1, c_loc(num_configs))
  if (status == 0 .or. num_configs == 0) then
    print *, "Failed to choose EGL config"
    stop 1
  end if
  
  ! Create context
  context_attribs = [int(z'30FB', c_int), 4, int(z'30FC', c_int), 3, EGL_NONE]  ! Version 4.3
  egl_context = eglCreateContext(egl_display, egl_config, c_null_ptr, c_loc(context_attribs))
  if (.not. c_associated(egl_context)) then
    print *, "Failed to create EGL context"
    stop 1
  end if
  
  ! Make current
  status = eglMakeCurrent(egl_display, c_null_ptr, c_null_ptr, egl_context)
  if (status == 0) then
    print *, "Failed to make EGL context current"
    stop 1
  end if
  
  print *, "✅ EGL context created"
  
  ! Generate GLSL shader
  block
    type(glsl_kernel_config) :: config
    
    config%M = M
    config%N = N
    config%K = K_dim
    config%tile_m = 4
    config%tile_n = 64
    config%tile_k = 4
    
    print *, ""
    print *, "Generating GLSL compute shader..."
    print *, "  M =", M, "(output channels)"
    print *, "  N =", N, "(output locations)"
    print *, "  K =", K_dim, "(input patch size)"
    
    shader_source = generate_conv_glsl_shader(config)
  end block
  
  ! Create and compile shader
  shader_id = glCreateShader(GL_COMPUTE_SHADER)
  if (shader_id == 0) then
    print *, "Failed to create shader"
    stop 1
  end if
  
  shader_ptr = c_loc(shader_source)
  call glShaderSource(shader_id, 1, c_loc(shader_ptr), c_null_ptr)
  call glCompileShader(shader_id)
  
  ! Check compilation
  call glGetShaderiv(shader_id, GL_COMPILE_STATUS, c_loc(status))
  if (status == 0) then
    call glGetShaderInfoLog(shader_id, 1024, c_null_ptr, c_loc(info_log))
    print *, "Shader compilation failed:"
    print *, trim(info_log)
    stop 1
  end if
  
  print *, "✅ Shader compiled successfully"
  
  ! Create program
  program_id = glCreateProgram()
  call glAttachShader(program_id, shader_id)
  call glLinkProgram(program_id)
  call glUseProgram(program_id)
  
  print *, "✅ Shader program created"
  
  ! Create buffers
  call glGenBuffers(3, c_loc(buffers))
  
  ! Allocate matrices
  allocate(A(M, K_dim))
  allocate(B(K_dim, N))
  allocate(C(M, N))
  allocate(C_ref(M, N))
  
  ! Initialize test data
  do i = 1, M
    do j = 1, K_dim
      A(i, j) = real(i + j) * 0.01
    end do
  end do
  
  do i = 1, K_dim
    do j = 1, N
      B(i, j) = sin(real(i * j) * 0.001)
    end do
  end do
  
  C = 0.0
  
  ! Upload to GPU
  print *, ""
  print *, "Uploading data to GPU..."
  
  ! Buffer A
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(M * K_dim * 4, c_size_t), &
                    c_loc(A), GL_DYNAMIC_COPY)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers(1))
  
  ! Buffer B
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(K_dim * N * 4, c_size_t), &
                    c_loc(B), GL_DYNAMIC_COPY)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers(2))
  
  ! Buffer C
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(M * N * 4, c_size_t), &
                    c_loc(C), GL_DYNAMIC_COPY)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers(3))
  
  print *, "✅ Data uploaded"
  
  ! Dispatch compute
  print *, ""
  print *, "Dispatching compute shader..."
  print *, "  Work groups:", (M + 3) / 4, "x", (N + 63) / 64
  
  call glDispatchCompute((M + 3) / 4, (N + 63) / 64, 1)
  call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
  call glFinish()
  
  print *, "✅ Compute completed"
  
  ! Read results
  print *, ""
  print *, "Reading results..."
  
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
  mapped_ptr = glMapBuffer(GL_SHADER_STORAGE_BUFFER, GL_READ_WRITE)
  if (c_associated(mapped_ptr)) then
    call c_f_pointer(mapped_ptr, C, [M, N])
    status = glUnmapBuffer(GL_SHADER_STORAGE_BUFFER)
  else
    print *, "Failed to map result buffer"
    stop 1
  end if
  
  ! Compute reference
  print *, ""
  print *, "Computing CPU reference..."
  
  do j = 1, N
    do i = 1, M
      C_ref(i, j) = 0.0
      do k = 1, K_dim
        C_ref(i, j) = C_ref(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
  
  ! Verify
  error = 0.0
  do i = 1, M
    do j = 1, N
      error = error + abs(C(i, j) - C_ref(i, j))
    end do
  end do
  
  error = error / (M * N)
  
  print *, ""
  print *, "Results (first 3x3):"
  do i = 1, min(3, M)
    do j = 1, min(3, N)
      print '(A,I2,A,I3,A,F8.4,A,F8.4)', &
        "  C[", i, ",", j, "] = ", C(i,j), " (ref: ", C_ref(i,j), ")"
    end do
  end do
  
  print *, ""
  print *, "Average error:", error
  
  if (error < 0.001) then
    print *, ""
    print *, "✅ SUCCESS! GLSL compute shader working for convolution!"
  else
    print *, "❌ Results don't match"
  end if
  
  ! Cleanup
  call glDeleteShader(shader_id)
  call glDeleteProgram(program_id)
  
  deallocate(A, B, C, C_ref)
  
end program test_glsl_gemm_compute