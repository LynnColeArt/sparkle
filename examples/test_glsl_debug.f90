program test_glsl_debug
  use iso_c_binding
  implicit none
  
  ! EGL constants
  integer(c_int32_t), parameter :: EGL_DEFAULT_DISPLAY = 0
  integer(c_int32_t), parameter :: EGL_NO_CONTEXT = 0
  integer(c_int32_t), parameter :: EGL_NO_SURFACE = 0
  integer(c_int32_t), parameter :: EGL_OPENGL_ES_API = int(z'30A0', c_int32_t)
  integer(c_int32_t), parameter :: EGL_CONTEXT_CLIENT_VERSION = int(z'3098', c_int32_t)
  integer(c_int32_t), parameter :: EGL_PBUFFER_BIT = int(z'0001', c_int32_t)
  integer(c_int32_t), parameter :: EGL_SURFACE_TYPE = int(z'3033', c_int32_t)
  integer(c_int32_t), parameter :: EGL_RENDERABLE_TYPE = int(z'3040', c_int32_t)
  integer(c_int32_t), parameter :: EGL_OPENGL_ES3_BIT = int(z'0040', c_int32_t)
  integer(c_int32_t), parameter :: EGL_NONE = int(z'3038', c_int32_t)
  integer(c_int32_t), parameter :: EGL_TRUE = 1
  
  ! GL constants
  integer(c_int), parameter :: GL_VERSION = int(z'1F02', c_int)
  integer(c_int), parameter :: GL_RENDERER = int(z'1F01', c_int)
  integer(c_int), parameter :: GL_SHADING_LANGUAGE_VERSION = int(z'8B8C', c_int)
  integer(c_int), parameter :: GL_COMPUTE_SHADER = int(z'91B9', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_DRAW = int(z'88E8', c_int)
  integer(c_int), parameter :: GL_MAP_READ_BIT = int(z'0001', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BARRIER_BIT = int(z'2000', c_int)
  integer(c_int), parameter :: GL_BUFFER_UPDATE_BARRIER_BIT = int(z'0200', c_int)
  integer(c_int), parameter :: GL_ALL_BARRIER_BITS = int(z'FFFFFFFF', c_int)
  integer(c_int), parameter :: GL_COMPILE_STATUS = int(z'8B81', c_int)
  integer(c_int), parameter :: GL_LINK_STATUS = int(z'8B82', c_int)
  integer(c_int), parameter :: GL_INFO_LOG_LENGTH = int(z'8B84', c_int)
  integer(c_int), parameter :: GL_TRUE = 1
  integer(c_int), parameter :: GL_FALSE = 0
  integer(c_int), parameter :: GL_NO_ERROR = 0
  
  ! EGL functions
  interface
    function eglGetDisplay(display_id) bind(C, name="eglGetDisplay")
      import :: c_ptr, c_int
      integer(c_int), value :: display_id
      type(c_ptr) :: eglGetDisplay
    end function
    
    function eglInitialize(dpy, major, minor) bind(C, name="eglInitialize")
      import :: c_ptr, c_int
      type(c_ptr), value :: dpy
      type(c_ptr), value :: major, minor
      integer(c_int) :: eglInitialize
    end function
    
    function eglBindAPI(api) bind(C, name="eglBindAPI")
      import :: c_int
      integer(c_int), value :: api
      integer(c_int) :: eglBindAPI
    end function
    
    function eglChooseConfig(dpy, attrib_list, configs, config_size, num_config) &
        bind(C, name="eglChooseConfig")
      import :: c_ptr, c_int
      type(c_ptr), value :: dpy
      type(c_ptr), value :: attrib_list, configs, num_config
      integer(c_int), value :: config_size
      integer(c_int) :: eglChooseConfig
    end function
    
    function eglCreateContext(dpy, config, share_context, attrib_list) &
        bind(C, name="eglCreateContext")
      import :: c_ptr
      type(c_ptr), value :: dpy, config, share_context, attrib_list
      type(c_ptr) :: eglCreateContext
    end function
    
    function eglMakeCurrent(dpy, draw, read, ctx) bind(C, name="eglMakeCurrent")
      import :: c_ptr, c_int
      type(c_ptr), value :: dpy, draw, read, ctx
      integer(c_int) :: eglMakeCurrent
    end function
  end interface
  
  ! GL functions
  interface
    function glGetString(name) bind(C, name="glGetString")
      import :: c_ptr, c_int
      integer(c_int), value :: name
      type(c_ptr) :: glGetString
    end function
    
    function glGetError() bind(C, name="glGetError")
      import :: c_int
      integer(c_int) :: glGetError
    end function
    
    function glCreateShader(shader_type) bind(C, name="glCreateShader")
      import :: c_int
      integer(c_int), value :: shader_type
      integer(c_int) :: glCreateShader
    end function
    
    subroutine glShaderSource(shader, count, string, length) bind(C, name="glShaderSource")
      import :: c_int, c_ptr
      integer(c_int), value :: shader, count
      type(c_ptr), value :: string, length
    end subroutine
    
    subroutine glCompileShader(shader) bind(C, name="glCompileShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    subroutine glGetShaderiv(shader, pname, params) bind(C, name="glGetShaderiv")
      import :: c_int, c_ptr
      integer(c_int), value :: shader, pname
      type(c_ptr), value :: params
    end subroutine
    
    function glCreateProgram() bind(C, name="glCreateProgram")
      import :: c_int
      integer(c_int) :: glCreateProgram
    end function
    
    subroutine glAttachShader(program, shader) bind(C, name="glAttachShader")
      import :: c_int
      integer(c_int), value :: program, shader
    end subroutine
    
    subroutine glLinkProgram(program) bind(C, name="glLinkProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glGetProgramiv(program, pname, params) bind(C, name="glGetProgramiv")
      import :: c_int, c_ptr
      integer(c_int), value :: program, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glUseProgram(program) bind(C, name="glUseProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glGenBuffers(n, buffers) bind(C, name="glGenBuffers")
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: buffers
    end subroutine
    
    subroutine glBindBuffer(target, buffer) bind(C, name="glBindBuffer")
      import :: c_int
      integer(c_int), value :: target, buffer
    end subroutine
    
    subroutine glBufferData(target, size, data, usage) bind(C, name="glBufferData")
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target, usage
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
    end subroutine
    
    subroutine glBindBufferBase(target, index, buffer) bind(C, name="glBindBufferBase")
      import :: c_int
      integer(c_int), value :: target, index, buffer
    end subroutine
    
    subroutine glDispatchCompute(num_groups_x, num_groups_y, num_groups_z) &
        bind(C, name="glDispatchCompute")
      import :: c_int
      integer(c_int), value :: num_groups_x, num_groups_y, num_groups_z
    end subroutine
    
    subroutine glMemoryBarrier(barriers) bind(C, name="glMemoryBarrier")
      import :: c_int
      integer(c_int), value :: barriers
    end subroutine
    
    subroutine glFinish() bind(C, name="glFinish")
    end subroutine
    
    function glMapBufferRange(target, offset, length, access) bind(C, name="glMapBufferRange")
      import :: c_ptr, c_int, c_size_t
      integer(c_int), value :: target, access
      integer(c_size_t), value :: offset, length
      type(c_ptr) :: glMapBufferRange
    end function
  end interface
  
  ! Local variables
  type(c_ptr) :: display, context
  type(c_ptr), target :: config
  type(c_ptr), parameter :: EGL_NO_CONTEXT_PTR = c_null_ptr
  type(c_ptr), parameter :: EGL_NO_SURFACE_PTR = c_null_ptr
  integer(c_int32_t), target :: egl_attribs(7), ctx_attribs(3)
  integer(c_int32_t), target :: num_configs
  integer(c_int), target :: status, shader_id, program_id, ssbo, gl_err
  integer(c_int), target :: compile_status, link_status
  character(kind=c_char), target :: shader_source(512)
  character(len=512) :: shader_str
  type(c_ptr) :: shader_ptr, mapped_ptr
  type(c_ptr), target :: shader_sources(1)
  integer(c_int32_t), pointer :: result_ptr(:)
  integer :: i, errors
  integer, parameter :: N = 1024
  
  print *, "=== Testing GLSL Compute with Debug Info ==="
  print *, ""
  
  ! Initialize EGL
  print *, "Initializing EGL..."
  display = eglGetDisplay(EGL_DEFAULT_DISPLAY)
  
  status = eglInitialize(display, c_null_ptr, c_null_ptr)
  if (status /= EGL_TRUE) stop "Failed to initialize EGL"
  
  status = eglBindAPI(EGL_OPENGL_ES_API)
  if (status /= EGL_TRUE) stop "Failed to bind OpenGL ES API"
  
  egl_attribs = [EGL_SURFACE_TYPE, EGL_PBUFFER_BIT, &
                 EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT, &
                 EGL_NONE, 0, 0]
  
  status = eglChooseConfig(display, c_loc(egl_attribs), c_loc(config), 1, c_loc(num_configs))
  if (status /= EGL_TRUE .or. num_configs == 0) stop "Failed to choose EGL config"
  
  ctx_attribs = [EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE]
  context = eglCreateContext(display, config, EGL_NO_CONTEXT_PTR, c_loc(ctx_attribs))
  if (.not. c_associated(context)) stop "Failed to create context"
  
  status = eglMakeCurrent(display, EGL_NO_SURFACE_PTR, EGL_NO_SURFACE_PTR, context)
  if (status /= EGL_TRUE) stop "Failed to make context current"
  
  print *, "✅ EGL context created"
  
  ! Print GL info
  block
    type(c_ptr) :: version_ptr
    character(kind=c_char), pointer :: str_ptr(:)
    character(len=256) :: version_str
    integer :: j
    
    ! GL Version
    version_ptr = glGetString(GL_VERSION)
    if (c_associated(version_ptr)) then
      call c_f_pointer(version_ptr, str_ptr, [256])
      version_str = ""
      do j = 1, 256
        if (str_ptr(j) == c_null_char) exit
        version_str(j:j) = str_ptr(j)
      end do
      print *, "  GL Version: ", trim(version_str)
    else
      print *, "  GL Version: <null>"
    end if
    
    ! GL Renderer
    version_ptr = glGetString(GL_RENDERER)
    if (c_associated(version_ptr)) then
      call c_f_pointer(version_ptr, str_ptr, [256])
      version_str = ""
      do j = 1, 256
        if (str_ptr(j) == c_null_char) exit
        version_str(j:j) = str_ptr(j)
      end do
      print *, "  GL Renderer: ", trim(version_str)
    else
      print *, "  GL Renderer: <null>"
    end if
    
    ! GLSL Version
    version_ptr = glGetString(GL_SHADING_LANGUAGE_VERSION)
    if (c_associated(version_ptr)) then
      call c_f_pointer(version_ptr, str_ptr, [256])
      version_str = ""
      do j = 1, 256
        if (str_ptr(j) == c_null_char) exit
        version_str(j:j) = str_ptr(j)
      end do
      print *, "  GLSL Version: ", trim(version_str)
    else
      print *, "  GLSL Version: <null>"
    end if
  end block
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after version queries:", gl_err
  end if
  
  print *, ""
  
  ! Create shader
  print *, "Creating compute shader..."
  shader_id = glCreateShader(GL_COMPUTE_SHADER)
  if (shader_id == 0) stop "Failed to create shader"
  
  ! Mini's minimal shader
  shader_str = &
    "#version 310 es" // NEW_LINE('A') // &
    "layout(local_size_x = 64) in;" // NEW_LINE('A') // &
    "layout(std430, binding = 0) buffer Out {" // NEW_LINE('A') // &
    "    uint data[];" // NEW_LINE('A') // &
    "};" // NEW_LINE('A') // &
    "void main() {" // NEW_LINE('A') // &
    "    uint i = gl_GlobalInvocationID.x;" // NEW_LINE('A') // &
    "    data[i] = 0xDEADBEEFu;" // NEW_LINE('A') // &
    "}" // C_NULL_CHAR
  
  do i = 1, len_trim(shader_str)
    shader_source(i) = shader_str(i:i)
  end do
  shader_source(len_trim(shader_str) + 1) = c_null_char
  
  shader_sources(1) = c_loc(shader_source)
  call glShaderSource(shader_id, 1, c_loc(shader_sources), c_null_ptr)
  
  call glCompileShader(shader_id)
  
  call glGetShaderiv(shader_id, GL_COMPILE_STATUS, c_loc(compile_status))
  if (compile_status /= GL_TRUE) then
    print *, "❌ Shader compilation failed"
    stop 1
  end if
  print *, "✅ Shader compiled"
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after shader compile:", gl_err
  end if
  
  ! Create program
  program_id = glCreateProgram()
  call glAttachShader(program_id, shader_id)
  call glLinkProgram(program_id)
  
  call glGetProgramiv(program_id, GL_LINK_STATUS, c_loc(link_status))
  if (link_status /= GL_TRUE) then
    print *, "❌ Program linking failed"
    stop 1
  end if
  print *, "✅ Shader program ready"
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after program link:", gl_err
  end if
  
  print *, ""
  
  ! Create SSBO
  print *, "Creating shader storage buffer..."
  call glGenBuffers(1, c_loc(ssbo))
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo)
  
  ! Allocate and poison buffer
  print *, "Poisoning buffer with 0xBAD0BAD0..."
  block
    integer(c_int32_t), target :: poison_data(N)
    poison_data = int(z'BAD0BAD0', c_int32_t)
    call glBufferData(GL_SHADER_STORAGE_BUFFER, int(N * 4, c_size_t), &
                      c_loc(poison_data), GL_DYNAMIC_DRAW)
  end block
  print *, "✅ Data uploaded to GPU"
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after buffer setup:", gl_err
  end if
  
  print *, ""
  
  ! Dispatch
  print *, "Dispatching compute shader..."
  call glUseProgram(program_id)
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after glUseProgram:", gl_err
  end if
  
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, ssbo)
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after glBindBufferBase:", gl_err
  end if
  
  print *, "  Work groups:", (N + 63) / 64
  print *, "  Total invocations:", (N + 63) / 64 * 64
  
  call glDispatchCompute((N + 63) / 64, 1, 1)
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after glDispatchCompute:", gl_err
  end if
  
  call glMemoryBarrier(GL_ALL_BARRIER_BITS)
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after glMemoryBarrier:", gl_err
  end if
  
  call glFinish()
  
  gl_err = glGetError()
  if (gl_err /= GL_NO_ERROR) then
    print *, "  ❌ GL Error after glFinish:", gl_err
  end if
  
  print *, "✅ Compute completed"
  
  print *, ""
  
  ! Read results
  print *, "Reading results..."
  mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, &
                                int(N * 4, c_size_t), GL_MAP_READ_BIT)
  
  if (.not. c_associated(mapped_ptr)) then
    print *, "❌ Failed to map buffer"
    stop 1
  end if
  
  call c_f_pointer(mapped_ptr, result_ptr, [N])
  
  print *, ""
  print *, "Results (first 10):"
  errors = 0
  do i = 1, min(10, N)
    if (result_ptr(i) /= int(z'DEADBEEF', c_int32_t)) then
      print '(A,I4,A,Z8,A)', "  [", i, "] = 0x", result_ptr(i), " (expected: 0xDEADBEEF)"
      errors = errors + 1
    else
      print '(A,I4,A,Z8,A)', "  [", i, "] = 0x", result_ptr(i), " ✅"
    end if
  end do
  
  ! Count total errors
  errors = 0
  do i = 1, N
    if (result_ptr(i) /= int(z'DEADBEEF', c_int32_t)) errors = errors + 1
  end do
  
  print *, ""
  print *, "Errors:", errors, "out of", N
  
  if (errors > 0) then
    print *, ""
    print *, "❌ Some values unchanged - shader execution issue"
  else
    print *, ""
    print *, "✅ All values correct - shader executed successfully!"
  end if
  
end program test_glsl_debug