program test_glsl_simple
  use iso_c_binding
  implicit none
  
  interface
    ! Minimal OpenGL functions we need
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
      type(c_ptr), dimension(*), intent(in) :: string
      type(c_ptr), value :: length
    end subroutine
    
    subroutine glCompileShader(shader) bind(C, name="glCompileShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glGetShaderiv(shader, pname, params) bind(C, name="glGetShaderiv")
      import :: c_int
      integer(c_int), value :: shader, pname
      integer(c_int), intent(out) :: params
    end subroutine
    
    subroutine glGetShaderInfoLog(shader, maxLength, length, infoLog) bind(C, name="glGetShaderInfoLog")
      import :: c_int, c_char
      integer(c_int), value :: shader, maxLength
      integer(c_int), intent(out) :: length
      character(c_char), dimension(*), intent(out) :: infoLog
    end subroutine
    
    subroutine glAttachShader(program, shader) bind(C, name="glAttachShader")
      import :: c_int
      integer(c_int), value :: program, shader
    end subroutine
    
    subroutine glLinkProgram(program) bind(C, name="glLinkProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glGetProgramiv(program, pname, params) bind(C, name="glGetProgramiv")
      import :: c_int
      integer(c_int), value :: program, pname
      integer(c_int), intent(out) :: params
    end subroutine
    
    subroutine glUseProgram(program) bind(C, name="glUseProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glGenBuffers(n, buffers) bind(C, name="glGenBuffers")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), dimension(*), intent(out) :: buffers
    end subroutine
    
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
    
    subroutine glBufferSubData(target, offset, size, data) bind(C, name="glBufferSubData")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: target
      integer(c_size_t), value :: offset
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
    end subroutine
    
    subroutine glBindBufferBase(target, index, buffer) bind(C, name="glBindBufferBase")
      import :: c_int
      integer(c_int), value :: target, index, buffer
    end subroutine
    
    subroutine glGetBufferSubData(target, offset, size, data) bind(C, name="glGetBufferSubData")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: target
      integer(c_size_t), value :: offset, size
      type(c_ptr), value :: data
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
    
    ! EGL functions
    function eglGetDisplay(display_id) bind(C, name="eglGetDisplay")
      import :: c_ptr
      type(c_ptr), value :: display_id
      type(c_ptr) :: eglGetDisplay
    end function
    
    function eglInitialize(display, major, minor) bind(C, name="eglInitialize")
      import :: c_ptr, c_int
      type(c_ptr), value :: display
      integer(c_int), intent(out) :: major, minor
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
      type(c_ptr), value :: display
      integer(c_int), dimension(*), intent(in) :: attrib_list
      type(c_ptr), dimension(*), intent(out) :: configs
      integer(c_int), value :: config_size
      integer(c_int), intent(out) :: num_config
      integer(c_int) :: eglChooseConfig
    end function
    
    function eglCreateContext(display, config, share_context, attrib_list) &
        bind(C, name="eglCreateContext")
      import :: c_ptr, c_int
      type(c_ptr), value :: display, config, share_context
      integer(c_int), dimension(*), intent(in) :: attrib_list
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
  integer(c_int), parameter :: GL_LINK_STATUS = int(z'8B82', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_STATIC_DRAW = int(z'88E4', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BARRIER_BIT = int(z'2000', c_int)
  
  ! EGL constants
  integer(c_int), parameter :: EGL_DEFAULT_DISPLAY = 0
  integer(c_int), parameter :: EGL_OPENGL_ES_API = int(z'30A0', c_int)
  integer(c_int), parameter :: EGL_OPENGL_ES2_BIT = 4
  integer(c_int), parameter :: EGL_NONE = int(z'3038', c_int)
  
  ! Variables
  type(c_ptr) :: egl_display, egl_context
  type(c_ptr) :: egl_configs(1)
  integer(c_int) :: shader_id, program_id
  integer(c_int), target :: ssbo_array(1)
  integer(c_int) :: ssbo
  integer(c_int) :: status, major, minor, num_configs
  integer(c_int) :: config_attribs(7)
  integer(c_int) :: context_attribs(5)
  character(len=1024) :: info_log
  integer(c_int) :: log_length
  integer :: i
  
  ! Test data
  integer, parameter :: N = 1024
  integer(c_int32_t), target :: data(N), expected
  character(len=2048), target :: shader_source
  type(c_ptr) :: shader_ptr(1)
  integer :: error_count
  
  ! Mini's minimal test shader - OpenGL ES version
  shader_source = &
    "#version 310 es" // NEW_LINE('A') // &
    "layout(local_size_x = 64) in;" // NEW_LINE('A') // &
    "layout(std430, binding = 0) buffer Out {" // NEW_LINE('A') // &
    "    uint data[];" // NEW_LINE('A') // &
    "};" // NEW_LINE('A') // &
    "void main() {" // NEW_LINE('A') // &
    "    uint i = gl_GlobalInvocationID.x;" // NEW_LINE('A') // &
    "    data[i] = 0xDEADBEEFu;" // NEW_LINE('A') // &
    "}" // C_NULL_CHAR
  
  print *, "=== Testing Simple GLSL Compute Shader ==="
  print *, ""
  
  ! Initialize EGL
  print *, "Initializing EGL..."
  egl_display = eglGetDisplay(c_null_ptr)
  if (.not. c_associated(egl_display)) stop "Failed to get display"
  
  status = eglInitialize(egl_display, major, minor)
  if (status == 0) stop "Failed to initialize EGL"
  print *, "  EGL version:", major, ".", minor
  
  status = eglBindAPI(EGL_OPENGL_ES_API)
  if (status == 0) stop "Failed to bind OpenGL ES API"
  
  ! Config attributes - request OpenGL ES 2.0 support
  config_attribs = [int(z'3024', c_int), EGL_OPENGL_ES2_BIT, &  ! EGL_RENDERABLE_TYPE
                    int(z'3040', c_int), 1, &                    ! EGL_CONTEXT_CLIENT_VERSION
                    EGL_NONE, 0, 0]
  
  status = eglChooseConfig(egl_display, config_attribs, egl_configs, 1, num_configs)
  if (status == 0) stop "Failed to choose config"
  
  ! Request OpenGL ES 3.2 for compute shader support
  context_attribs = [int(z'3098', c_int), 3, int(z'30FB', c_int), 2, EGL_NONE]
  egl_context = eglCreateContext(egl_display, egl_configs(1), c_null_ptr, context_attribs)
  if (.not. c_associated(egl_context)) stop "Failed to create context"
  
  status = eglMakeCurrent(egl_display, c_null_ptr, c_null_ptr, egl_context)
  if (status == 0) stop "Failed to make context current"
  
  print *, "✅ EGL context created"
  
  ! Print GL version to verify compute support
  block
    interface
      function glGetString(name) bind(C, name="glGetString")
        import :: c_int, c_ptr
        integer(c_int), value :: name
        type(c_ptr) :: glGetString
      end function
    end interface
    
    integer(c_int), parameter :: GL_VERSION = int(z'1F02', c_int)
    integer(c_int), parameter :: GL_SHADING_LANGUAGE_VERSION = int(z'8B8C', c_int)
    type(c_ptr) :: version_ptr
    character(len=256) :: version_str
    integer :: str_len
    
    ! For now, just check if we got pointers
    version_ptr = glGetString(GL_VERSION)
    if (c_associated(version_ptr)) then
      print *, "  GL Version: (got valid pointer)"
    else
      print *, "  GL Version: <null>"
    end if
    
    version_ptr = glGetString(GL_SHADING_LANGUAGE_VERSION)
    if (c_associated(version_ptr)) then
      print *, "  GLSL Version: (got valid pointer)"
    else
      print *, "  GLSL Version: <null>"
    end if
  end block
  
  print *, ""
  
  ! Create shader
  print *, "Creating compute shader..."
  shader_id = glCreateShader(GL_COMPUTE_SHADER)
  if (shader_id == 0) stop "Failed to create shader"
  
  shader_ptr(1) = c_loc(shader_source)
  call glShaderSource(shader_id, 1, shader_ptr, c_null_ptr)
  call glCompileShader(shader_id)
  
  call glGetShaderiv(shader_id, GL_COMPILE_STATUS, status)
  if (status == 0) then
    call glGetShaderInfoLog(shader_id, 1024, log_length, info_log)
    print *, "Shader compilation failed:"
    print *, info_log(1:log_length)
    stop
  end if
  
  print *, "✅ Shader compiled"
  
  ! Create program
  program_id = glCreateProgram()
  call glAttachShader(program_id, shader_id)
  call glLinkProgram(program_id)
  
  call glGetProgramiv(program_id, GL_LINK_STATUS, status)
  if (status == 0) then
    print *, "Program linking failed"
    stop
  end if
  
  call glUseProgram(program_id)
  print *, "✅ Shader program ready"
  print *, ""
  
  ! Create single SSBO
  print *, "Creating shader storage buffer..."
  call glGenBuffers(1, ssbo_array)
  ssbo = ssbo_array(1)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(N * 4, c_size_t), c_null_ptr, GL_STATIC_DRAW)
  
  ! Poison with BAD0BAD0 to see changes
  print *, "Poisoning buffer with 0xBAD0BAD0..."
  do i = 1, N
    data(i) = int(z'BAD0BAD0', c_int32_t)
  end do
  call glBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, int(N * 4, c_size_t), c_loc(data))
  
  ! Bind at binding=0 to match layout(binding=0)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, ssbo)
  
  print *, "✅ Data uploaded to GPU"
  print *, ""
  
  ! Dispatch compute
  print *, "Dispatching compute shader..."
  print *, "  Work groups:", (N + 63) / 64
  print *, "  Total invocations:", (N + 63) / 64 * 64
  
  call glDispatchCompute((N + 63) / 64, 1, 1)
  
  ! THE BIGGEST GOTCHA - Mini's critical barrier
  call glMemoryBarrier(ior(GL_SHADER_STORAGE_BARRIER_BIT, int(z'0002', c_int))) ! GL_BUFFER_UPDATE_BARRIER_BIT
  
  ! Sync
  call glFinish()
  
  print *, "✅ Compute completed"
  print *, ""
  
  ! Read results
  print *, "Reading results..."
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo)
  call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, int(N * 4, c_size_t), c_loc(data))
  
  ! Verify
  expected = int(z'DEADBEEF', c_int32_t)
  error_count = 0
  
  print *, ""
  print *, "Results (first 10):"
  do i = 1, 10
    print '(A,I4,A,Z8,A,Z8,A)', &
      "  [", i, "] = 0x", data(i), " (expected: 0x", expected, ")"
    if (data(i) /= expected) error_count = error_count + 1
  end do
  
  ! Count total errors
  do i = 11, N
    if (data(i) /= expected) error_count = error_count + 1
  end do
  
  print *, ""
  print *, "Errors:", error_count, "out of", N
  
  if (error_count == 0) then
    print *, ""
    print *, "✅ SUCCESS! GLSL compute shader executed on GPU!"
    print *, ""
    print *, "Mini was right - the pipeline wasn't armed!"
    print *, "Hardware and driver are fine!"
  else if (error_count == N) then
    print *, ""
    print *, "❌ All values unchanged - shader didn't execute"
    print *, "Still seeing 0xBAD0BAD0 everywhere"
  else
    print *, ""
    print *, "⚠️ Partial execution - some values changed"
  end if
  
end program test_glsl_simple