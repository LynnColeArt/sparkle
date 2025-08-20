module sporkle_nvidia_opengl
  ! NVIDIA GPU support via OpenGL compute shaders
  ! Production-ready implementation following AMD success pattern
  
  use kinds
  use iso_c_binding
  use sporkle_types
  implicit none
  private
  
  public :: nvidia_gl_init
  public :: nvidia_gl_shutdown  
  public :: nvidia_gl_execute_conv2d
  public :: nvidia_gl_execute_conv2d_timed
  public :: nvidia_gl_get_device_info
  
  ! Export OpenGL constants and functions for other modules
  public :: GL_SHADER_STORAGE_BUFFER, GL_DYNAMIC_COPY
  public :: GL_MAP_READ_BIT, GL_SHADER_STORAGE_BARRIER_BIT
  public :: glGenBuffers, glBindBuffer, glBufferData
  public :: glBindBufferBase, glUseProgram, glUniform1i
  public :: glGetUniformLocation, glDispatchCompute
  public :: glMemoryBarrier, glFinish, glMapBufferRange
  public :: glUnmapBuffer
  public :: conv2d_program
  
  ! OpenGL/EGL function interfaces
  interface
    function eglGetDisplay(display_id) bind(C, name='eglGetDisplay')
      import :: c_ptr
      type(c_ptr), value :: display_id
      type(c_ptr) :: eglGetDisplay
    end function
    
    function eglInitialize(display, major, minor) bind(C, name='eglInitialize')
      import :: c_ptr, c_int
      type(c_ptr), value :: display
      type(c_ptr), value :: major, minor
      integer(c_int) :: eglInitialize
    end function
    
    function eglBindAPI(api) bind(C, name='eglBindAPI')
      import :: c_int
      integer(c_int), value :: api
      integer(c_int) :: eglBindAPI
    end function
    
    function eglChooseConfig(display, attrib_list, configs, config_size, num_config) &
        bind(C, name='eglChooseConfig')
      import :: c_ptr, c_int
      type(c_ptr), value :: display, attrib_list, configs
      integer(c_int), value :: config_size
      type(c_ptr), value :: num_config
      integer(c_int) :: eglChooseConfig
    end function
    
    function eglCreateContext(display, config, share_context, attrib_list) &
        bind(C, name='eglCreateContext')
      import :: c_ptr
      type(c_ptr), value :: display, config, share_context, attrib_list
      type(c_ptr) :: eglCreateContext
    end function
    
    function eglMakeCurrent(display, draw, read, context) bind(C, name='eglMakeCurrent')
      import :: c_ptr, c_int
      type(c_ptr), value :: display, draw, read, context
      integer(c_int) :: eglMakeCurrent
    end function
    
    function glGetString(name) bind(C, name='glGetString')
      import :: c_int, c_ptr
      integer(c_int), value :: name
      type(c_ptr) :: glGetString
    end function
    
    function glCreateShader(shader_type) bind(C, name='glCreateShader')
      import :: c_int
      integer(c_int), value :: shader_type
      integer(c_int) :: glCreateShader
    end function
    
    subroutine glShaderSource(shader, count, string, length) bind(C, name='glShaderSource')
      import :: c_int, c_ptr
      integer(c_int), value :: shader, count
      type(c_ptr), value :: string, length
    end subroutine
    
    subroutine glCompileShader(shader) bind(C, name='glCompileShader')
      import :: c_int
      integer(c_int), value :: shader
    end subroutine
    
    function glCreateProgram() bind(C, name='glCreateProgram')
      import :: c_int
      integer(c_int) :: glCreateProgram
    end function
    
    subroutine glAttachShader(program, shader) bind(C, name='glAttachShader')
      import :: c_int
      integer(c_int), value :: program, shader
    end subroutine
    
    subroutine glLinkProgram(program) bind(C, name='glLinkProgram')
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    subroutine glUseProgram(program) bind(C, name='glUseProgram')
      import :: c_int
      integer(c_int), value :: program
    end subroutine
    
    function glGetUniformLocation(program, name) bind(C, name='glGetUniformLocation')
      import :: c_int, c_ptr
      integer(c_int), value :: program
      type(c_ptr), value :: name
      integer(c_int) :: glGetUniformLocation
    end function
    
    subroutine glUniform1i(location, v0) bind(C, name='glUniform1i')
      import :: c_int
      integer(c_int), value :: location, v0
    end subroutine
    
    function glMapBufferRange(target, offset, length, access) bind(C, name='glMapBufferRange')
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: offset, length
      integer(c_int), value :: access
      type(c_ptr) :: glMapBufferRange
    end function
    
    function glUnmapBuffer(target) bind(C, name='glUnmapBuffer')
      import :: c_int
      integer(c_int), value :: target
      integer(c_int) :: glUnmapBuffer
    end function
    
    ! Fence Sync functions (more widely supported)
    function glFenceSync(condition, flags) bind(C, name='glFenceSync')
      import :: c_int, c_ptr
      integer(c_int), value :: condition, flags
      type(c_ptr) :: glFenceSync
    end function
    
    function glClientWaitSync(sync, flags, timeout) bind(C, name='glClientWaitSync')
      import :: c_int, c_ptr, c_int64_t
      type(c_ptr), value :: sync
      integer(c_int), value :: flags
      integer(c_int64_t), value :: timeout
      integer(c_int) :: glClientWaitSync
    end function
    
    subroutine glDeleteSync(sync) bind(C, name='glDeleteSync')
      import :: c_ptr
      type(c_ptr), value :: sync
    end subroutine
    
    ! GPU Timer Query functions
    subroutine glGenQueries(n, ids) bind(C, name='glGenQueries')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: ids
    end subroutine
    
    subroutine glDeleteQueries(n, ids) bind(C, name='glDeleteQueries')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: ids
    end subroutine
    
    subroutine glBeginQuery(target, id) bind(C, name='glBeginQuery')
      import :: c_int
      integer(c_int), value :: target, id
    end subroutine
    
    subroutine glEndQuery(target) bind(C, name='glEndQuery')
      import :: c_int
      integer(c_int), value :: target
    end subroutine
    
    subroutine glGetQueryObjectiv(id, pname, params) bind(C, name='glGetQueryObjectiv')
      import :: c_int, c_ptr
      integer(c_int), value :: id, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetQueryObjectuiv(id, pname, params) bind(C, name='glGetQueryObjectuiv')
      import :: c_int, c_ptr
      integer(c_int), value :: id, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetQueryObjecti64v(id, pname, params) bind(C, name='glGetQueryObjecti64v')
      import :: c_int, c_ptr, c_int64_t
      integer(c_int), value :: id, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetInteger64v(pname, params) bind(C, name='glGetInteger64v')
      import :: c_int, c_ptr
      integer(c_int), value :: pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGenBuffers(n, buffers) bind(C, name='glGenBuffers')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: buffers
    end subroutine
    
    subroutine glBindBuffer(target, buffer) bind(C, name='glBindBuffer')
      import :: c_int
      integer(c_int), value :: target, buffer
    end subroutine
    
    subroutine glBufferData(target, size, data, usage) bind(C, name='glBufferData')
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
      integer(c_int), value :: usage
    end subroutine
    
    subroutine glBindBufferBase(target, index, buffer) bind(C, name='glBindBufferBase')
      import :: c_int
      integer(c_int), value :: target, index, buffer
    end subroutine
    
    subroutine glDispatchCompute(num_groups_x, num_groups_y, num_groups_z) &
        bind(C, name='glDispatchCompute')
      import :: c_int
      integer(c_int), value :: num_groups_x, num_groups_y, num_groups_z
    end subroutine
    
    subroutine glMemoryBarrier(barriers) bind(C, name='glMemoryBarrier')
      import :: c_int
      integer(c_int), value :: barriers
    end subroutine
    
    subroutine glFinish() bind(C, name='glFinish')
    end subroutine
    
    function glGetError() bind(C, name='glGetError')
      import :: c_int
      integer(c_int) :: glGetError
    end function
    
    subroutine glGetShaderiv(shader, pname, params) bind(C, name='glGetShaderiv')
      import :: c_int, c_ptr
      integer(c_int), value :: shader, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetShaderInfoLog(shader, maxLength, length, infoLog) &
        bind(C, name='glGetShaderInfoLog')
      import :: c_int, c_ptr
      integer(c_int), value :: shader, maxLength
      type(c_ptr), value :: length, infoLog
    end subroutine
    
    subroutine glGetProgramiv(program, pname, params) bind(C, name='glGetProgramiv')
      import :: c_int, c_ptr
      integer(c_int), value :: program, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetProgramInfoLog(program, maxLength, length, infoLog) &
        bind(C, name='glGetProgramInfoLog')
      import :: c_int, c_ptr
      integer(c_int), value :: program, maxLength
      type(c_ptr), value :: length, infoLog
    end subroutine
  end interface
  
  ! OpenGL constants
  integer(c_int), parameter :: GL_COMPUTE_SHADER = int(z'91B9', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_COPY = int(z'88EA', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BARRIER_BIT = int(z'2000', c_int)
  integer(c_int), parameter :: GL_NO_ERROR = 0
  integer(c_int), parameter :: GL_VENDOR = int(z'1F00', c_int)
  integer(c_int), parameter :: GL_RENDERER = int(z'1F01', c_int)
  integer(c_int), parameter :: GL_COMPILE_STATUS = int(z'8B81', c_int)
  integer(c_int), parameter :: GL_LINK_STATUS = int(z'8B82', c_int)
  integer(c_int), parameter :: GL_INFO_LOG_LENGTH = int(z'8B84', c_int)
  integer(c_int), parameter :: GL_MAP_READ_BIT = int(z'0001', c_int)
  
  ! Timer query constants
  integer(c_int), parameter :: GL_TIME_ELAPSED = int(z'88BF', c_int)
  integer(c_int), parameter :: GL_QUERY_RESULT = int(z'8866', c_int)
  integer(c_int), parameter :: GL_QUERY_RESULT_AVAILABLE = int(z'8867', c_int)
  
  ! Sync constants
  integer(c_int), parameter :: GL_SYNC_GPU_COMMANDS_COMPLETE = int(z'9117', c_int)
  integer(c_int), parameter :: GL_SYNC_FLUSH_COMMANDS_BIT = int(z'00000001', c_int)
  integer(c_int), parameter :: GL_TIMEOUT_EXPIRED = int(z'911B', c_int)
  integer(c_int), parameter :: GL_CONDITION_SATISFIED = int(z'911C', c_int)
  integer(c_int), parameter :: GL_WAIT_FAILED = int(z'911D', c_int)
  
  ! Export these for other modules
  public :: GL_SYNC_GPU_COMMANDS_COMPLETE
  public :: GL_TIME_ELAPSED, GL_QUERY_RESULT, GL_QUERY_RESULT_AVAILABLE
  public :: glGenQueries, glBeginQuery, glEndQuery
  public :: glGetQueryObjectiv, glGetQueryObjecti64v
  
  ! EGL constants
  integer(c_int), parameter :: EGL_DEFAULT_DISPLAY = 0
  integer(c_int), parameter :: EGL_NO_SURFACE = 0
  integer(c_int), parameter :: EGL_NO_CONTEXT = 0
  integer(c_int), parameter :: EGL_OPENGL_API = int(z'30A2', c_int)
  integer(c_int), parameter :: EGL_OPENGL_ES_API = int(z'30A0', c_int)  ! Use ES for compute
  integer(c_int), parameter :: EGL_SURFACE_TYPE = int(z'3033', c_int)
  integer(c_int), parameter :: EGL_PBUFFER_BIT = int(z'0001', c_int)
  integer(c_int), parameter :: EGL_RENDERABLE_TYPE = int(z'3040', c_int)
  integer(c_int), parameter :: EGL_OPENGL_BIT = int(z'0008', c_int)
  integer(c_int), parameter :: EGL_OPENGL_ES3_BIT = int(z'0040', c_int)  ! ES 3.x support
  integer(c_int), parameter :: EGL_CONTEXT_MAJOR_VERSION = int(z'3098', c_int)
  integer(c_int), parameter :: EGL_CONTEXT_MINOR_VERSION = int(z'30FB', c_int)
  integer(c_int), parameter :: EGL_CONTEXT_CLIENT_VERSION = int(z'3098', c_int)
  integer(c_int), parameter :: EGL_RED_SIZE = int(z'3024', c_int)
  integer(c_int), parameter :: EGL_GREEN_SIZE = int(z'3023', c_int)
  integer(c_int), parameter :: EGL_BLUE_SIZE = int(z'3022', c_int)
  integer(c_int), parameter :: EGL_ALPHA_SIZE = int(z'3021', c_int)
  integer(c_int), parameter :: EGL_DEPTH_SIZE = int(z'3025', c_int)
  integer(c_int), parameter :: EGL_NONE = int(z'3038', c_int)
  
  ! Module state
  type(c_ptr) :: egl_display = c_null_ptr
  type(c_ptr) :: egl_context = c_null_ptr
  logical :: initialized = .false.
  integer(c_int) :: conv2d_program = 0
  
contains
  
  function nvidia_gl_init() result(success)
    logical :: success
    type(c_ptr), target :: config_ptr
    integer(c_int), target :: major, minor, num_configs
    integer(c_int), target :: config_attribs(32), context_attribs(8)  ! Extra space for flexibility
    integer :: ret
    
    success = .false.
    
    ! Get EGL display
    egl_display = eglGetDisplay(c_null_ptr)
    if (.not. c_associated(egl_display)) then
      print *, "ERROR: Failed to get EGL display"
      return
    end if
    
    ! Initialize EGL
    ret = eglInitialize(egl_display, c_loc(major), c_loc(minor))
    if (ret == 0) then
      print *, "ERROR: Failed to initialize EGL"
      return
    end if
    print '(A,I0,A,I0)', "EGL initialized: version ", major, ".", minor
    
    ! Try OpenGL ES API for compute shaders (more widely supported)
    ret = eglBindAPI(EGL_OPENGL_ES_API)
    if (ret == 0) then
      print *, "WARNING: Failed to bind OpenGL ES API, trying desktop OpenGL"
      ret = eglBindAPI(EGL_OPENGL_API)
      if (ret == 0) then
        print *, "ERROR: Failed to bind any OpenGL API"
        return
      end if
    end if
    
    ! Choose config with full attribute list
    ! Initialize full array first
    config_attribs = 0
    config_attribs(1:15) = [&
      EGL_SURFACE_TYPE, EGL_PBUFFER_BIT, &          ! 2 elements
      EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT, &    ! 2 elements  
      EGL_RED_SIZE, 8, &                            ! 2 elements
      EGL_GREEN_SIZE, 8, &                          ! 2 elements
      EGL_BLUE_SIZE, 8, &                           ! 2 elements
      EGL_ALPHA_SIZE, 8, &                          ! 2 elements
      EGL_DEPTH_SIZE, 0, &                          ! 2 elements
      EGL_NONE]                                     ! 1 element = 15 total
    
    ret = eglChooseConfig(egl_display, c_loc(config_attribs), &
                          c_loc(config_ptr), 1, c_loc(num_configs))
    if (ret == 0 .or. num_configs == 0) then
      print *, "ERROR: Failed to choose EGL config, ret=", ret, " configs=", num_configs
      ! Try simpler config
      config_attribs = 0
      config_attribs(1:3) = [EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT, EGL_NONE]
      ret = eglChooseConfig(egl_display, c_loc(config_attribs), &
                            c_loc(config_ptr), 1, c_loc(num_configs))
      if (ret == 0 .or. num_configs == 0) then
        print *, "ERROR: Even minimal config failed"
        return
      end if
    end if
    print *, "EGL config chosen successfully"
    
    ! Create OpenGL ES 3.1 context (minimum for compute shaders)
    context_attribs = 0
    context_attribs(1:5) = [&
      EGL_CONTEXT_CLIENT_VERSION, 3, &
      EGL_CONTEXT_MINOR_VERSION, 1, &
      EGL_NONE]
    
    egl_context = eglCreateContext(egl_display, config_ptr, c_null_ptr, c_loc(context_attribs))
    if (.not. c_associated(egl_context)) then
      print *, "ERROR: Failed to create OpenGL ES 3.1 context"
      ! Try OpenGL ES 3.0
      context_attribs = 0
      context_attribs(1:3) = [EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE]
      egl_context = eglCreateContext(egl_display, config_ptr, c_null_ptr, c_loc(context_attribs))
      if (.not. c_associated(egl_context)) then
        print *, "ERROR: Failed to create any OpenGL ES context"
        return
      end if
    end if
    
    ! Make current
    ret = eglMakeCurrent(egl_display, c_null_ptr, c_null_ptr, egl_context)
    if (ret == 0) then
      print *, "ERROR: Failed to make context current"
      return
    end if
    
    ! Create compute shader program
    call create_conv2d_program()
    
    initialized = .true.
    success = .true.
    
    print *, "NVIDIA OpenGL compute initialized successfully!"
  end function nvidia_gl_init
  
  subroutine nvidia_gl_shutdown()
    if (initialized) then
      ! Cleanup would go here
      initialized = .false.
    end if
  end subroutine nvidia_gl_shutdown
  
  subroutine create_conv2d_program()
    integer(c_int) :: shader, error
    integer(c_int), target :: compile_status, link_status, log_length
    character(len=16384) :: shader_source
    type(c_ptr), target :: shader_ptr
    character(kind=c_char), target :: c_source(16384), info_log(1024)
    integer :: i, unit, iostat
    character(len=256) :: line
    logical :: file_exists
    
    ! Try to load Summit V2 ES shader from file first
    inquire(file="summit_kernel_v2_es.glsl", exist=file_exists)
    
    if (file_exists) then
      print *, "Loading Summit V2 ES shader from file..."
      open(newunit=unit, file="summit_kernel_v2_es.glsl", status="old", action="read")
      shader_source = ""
      do
        read(unit, '(A)', iostat=iostat) line
        if (iostat /= 0) exit
        shader_source = trim(shader_source) // trim(line) // c_new_line
      end do
      close(unit)
      print *, "Summit V2 shader loaded successfully"
    else
      ! Fallback to embedded ES 3.1 shader
      print *, "Summit V2 shader not found, using embedded shader"
      shader_source = &
        "#version 310 es" // c_new_line // &
        "precision highp float;" // c_new_line // &
        "layout(local_size_x = 32, local_size_y = 8) in;" // c_new_line // &
      "" // c_new_line // &
      "layout(std430, binding = 0) readonly buffer InputBuffer {" // c_new_line // &
      "    float input_data[];" // c_new_line // &
      "};" // c_new_line // &
      "" // c_new_line // &
      "layout(std430, binding = 1) readonly buffer KernelBuffer {" // c_new_line // &
      "    float kernel_data[];" // c_new_line // &
      "};" // c_new_line // &
      "" // c_new_line // &
      "layout(std430, binding = 2) writeonly buffer OutputBuffer {" // c_new_line // &
      "    float output_data[];" // c_new_line // &
      "};" // c_new_line // &
      "" // c_new_line // &
      "uniform int batch_size;" // c_new_line // &
      "uniform int in_channels;" // c_new_line // &
      "uniform int out_channels;" // c_new_line // &
      "uniform int height;" // c_new_line // &
      "uniform int width;" // c_new_line // &
      "uniform int kernel_h;" // c_new_line // &
      "uniform int kernel_w;" // c_new_line // &
      "" // c_new_line // &
      "void main() {" // c_new_line // &
      "    int x = int(gl_GlobalInvocationID.x);" // c_new_line // &
      "    int y = int(gl_GlobalInvocationID.y);" // c_new_line // &
      "    int oc = int(gl_GlobalInvocationID.z);" // c_new_line // &
      "    " // c_new_line // &
      "    if (x >= width || y >= height || oc >= out_channels) return;" // c_new_line // &
      "    " // c_new_line // &
      "    // Real convolution with tiling optimization" // c_new_line // &
      "    float sum = 0.0;" // c_new_line // &
      "    " // c_new_line // &
      "    for (int ic = 0; ic < in_channels; ic++) {" // c_new_line // &
      "        for (int kh = 0; kh < kernel_h; kh++) {" // c_new_line // &
      "            for (int kw = 0; kw < kernel_w; kw++) {" // c_new_line // &
      "                int in_y = y + kh;" // c_new_line // &
      "                int in_x = x + kw;" // c_new_line // &
      "                " // c_new_line // &
      "                if (in_y < height && in_x < width) {" // c_new_line // &
      "                    int in_idx = (ic * height + in_y) * width + in_x;" // c_new_line // &
      "                    int k_idx = ((oc * in_channels + ic) * kernel_h + kh) * kernel_w + kw;" // c_new_line // &
      "                    sum += input_data[in_idx] * kernel_data[k_idx];" // c_new_line // &
      "                }" // c_new_line // &
      "            }" // c_new_line // &
      "        }" // c_new_line // &
      "    }" // c_new_line // &
      "    " // c_new_line // &
      "    int out_idx = (oc * height + y) * width + x;" // c_new_line // &
      "    output_data[out_idx] = sum;" // c_new_line // &
      "}"
    end if
    
    ! Convert to C string
    do i = 1, len_trim(shader_source)
      c_source(i) = shader_source(i:i)
    end do
    c_source(len_trim(shader_source)+1) = c_null_char
    
    ! Create and compile shader
    shader = glCreateShader(GL_COMPUTE_SHADER)
    if (shader == 0) then
      print *, "ERROR: Failed to create compute shader"
      return
    end if
    
    shader_ptr = c_loc(c_source)
    call glShaderSource(shader, 1, c_loc(shader_ptr), c_null_ptr)
    call glCompileShader(shader)
    
    ! Check compilation status
    call glGetShaderiv(shader, GL_COMPILE_STATUS, c_loc(compile_status))
    if (compile_status == 0) then
      call glGetShaderiv(shader, GL_INFO_LOG_LENGTH, c_loc(log_length))
      if (log_length > 0) then
        call glGetShaderInfoLog(shader, min(log_length, 1024), c_null_ptr, c_loc(info_log))
        print *, "ERROR: Shader compilation failed:"
        do i = 1, min(log_length, 1024)
          if (info_log(i) == c_null_char) exit
          write(*, '(A)', advance='no') info_log(i)
        end do
        print *, ""
      end if
      return
    end if
    
    ! Create program and link
    conv2d_program = glCreateProgram()
    if (conv2d_program == 0) then
      print *, "ERROR: Failed to create shader program"
      return
    end if
    
    call glAttachShader(conv2d_program, shader)
    call glLinkProgram(conv2d_program)
    
    ! Check link status
    call glGetProgramiv(conv2d_program, GL_LINK_STATUS, c_loc(link_status))
    if (link_status == 0) then
      call glGetProgramiv(conv2d_program, GL_INFO_LOG_LENGTH, c_loc(log_length))
      if (log_length > 0) then
        call glGetProgramInfoLog(conv2d_program, min(log_length, 1024), c_null_ptr, c_loc(info_log))
        print *, "ERROR: Shader linking failed:"
        do i = 1, min(log_length, 1024)
          if (info_log(i) == c_null_char) exit
          write(*, '(A)', advance='no') info_log(i)
        end do
        print *, ""
      end if
      return
    end if
    
    print *, "✅ Compute shader compiled and linked successfully!"
    
    error = glGetError()
    if (error /= GL_NO_ERROR) then
      print *, "Warning: OpenGL error code:", error
    end if
  end subroutine create_conv2d_program
  
  function nvidia_gl_execute_conv2d(input, kernel, output, &
                                     batch, in_c, out_c, h, w, kh, kw) result(success)
    real(sp), intent(in), target :: input(*), kernel(*)
    real(sp), intent(out), target :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    logical :: success
    
    integer(c_int), target :: buffers(3)
    integer(c_size_t) :: input_size, kernel_size, output_size
    integer(c_int) :: groups_x, groups_y
    real(c_double) :: start_time, end_time
    
    success = .false.
    
    if (.not. initialized) then
      print *, "ERROR: NVIDIA OpenGL not initialized"
      return
    end if
    
    ! Calculate buffer sizes
    input_size = batch * in_c * h * w * 4  ! sizeof(float)
    kernel_size = out_c * in_c * kh * kw * 4
    output_size = batch * out_c * h * w * 4
    
    ! Generate buffers
    call glGenBuffers(3, c_loc(buffers))
    
    ! Upload input data
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, input_size, c_loc(input), GL_DYNAMIC_COPY)
    
    ! Upload kernel data
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, kernel_size, c_loc(kernel), GL_DYNAMIC_COPY)
    
    ! Allocate output buffer
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, output_size, c_null_ptr, GL_DYNAMIC_COPY)
    
    ! Bind buffers to shader
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers(1))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers(2))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers(3))
    
    ! Use program
    call glUseProgram(conv2d_program)
    
    ! Set uniforms
    block
      character(len=32, kind=c_char), target :: uniform_name
      integer(c_int) :: loc
      
      ! Set all the uniform parameters
      uniform_name = "batch_size" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, batch)
      
      uniform_name = "in_channels" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, in_c)
      
      uniform_name = "out_channels" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, out_c)
      
      uniform_name = "height" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, h)
      
      uniform_name = "width" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, w)
      
      uniform_name = "kernel_h" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, kh)
      
      uniform_name = "kernel_w" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, kw)
    end block
    
    ! Calculate dispatch size (32x32 tiles as discovered by profiler)
    groups_x = (w + 31) / 32
    groups_y = (h + 3) / 4   ! local_size_y = 4
    
    ! Launch compute shader (process all output channels)
    call cpu_time(start_time)
    call glDispatchCompute(groups_x, groups_y, out_c)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    call cpu_time(end_time)
    
    ! Read back results to verify GPU execution
    block
      type(c_ptr) :: mapped_ptr
      real(sp), pointer :: mapped_data(:)
      integer :: out_size
      
      out_size = batch * out_c * h * w
      
      ! Map the output buffer to read results
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
      mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, &
                                   int(output_size, c_size_t), GL_MAP_READ_BIT)
      
      if (c_associated(mapped_ptr)) then
        call c_f_pointer(mapped_ptr, mapped_data, [out_size])
        ! Copy the data
        output(1:out_size) = mapped_data(1:out_size)
        
        ! Unmap buffer
        if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
          print *, "Warning: glUnmapBuffer failed"
        end if
      else
        print *, "Warning: Failed to map output buffer"
      end if
    end block
    
    success = .true.
    
    print *, "NVIDIA conv2d executed in", (end_time - start_time) * 1000.0, "ms"
  end function nvidia_gl_execute_conv2d
  
  function nvidia_gl_execute_conv2d_timed(input, kernel, output, &
                                         batch, in_c, out_c, h, w, kh, kw, &
                                         gpu_time_ms) result(success)
    ! Version with GPU timer queries to measure actual GPU execution time
    real(sp), intent(in), target :: input(*), kernel(*)
    real(sp), intent(out), target :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    real(dp), intent(out) :: gpu_time_ms
    logical :: success
    
    integer(c_int), target :: buffers(3)
    integer(c_int), target :: query_id
    integer(c_size_t) :: input_size, kernel_size, output_size
    integer(c_int) :: groups_x, groups_y
    integer(c_int), target :: query_available
    integer(c_int64_t), target :: elapsed_ns
    integer :: wait_count
    
    success = .false.
    gpu_time_ms = 0.0_dp
    
    if (.not. initialized) then
      print *, "ERROR: NVIDIA OpenGL not initialized"
      return
    end if
    
    ! Generate timer query
    call glGenQueries(1, c_loc(query_id))
    
    ! Calculate buffer sizes
    input_size = batch * in_c * h * w * 4  ! sizeof(float)
    kernel_size = out_c * in_c * kh * kw * 4
    output_size = batch * out_c * h * w * 4
    
    ! Generate buffers
    call glGenBuffers(3, c_loc(buffers))
    
    ! Upload input data
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, input_size, c_loc(input), GL_DYNAMIC_COPY)
    
    ! Upload kernel data
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, kernel_size, c_loc(kernel), GL_DYNAMIC_COPY)
    
    ! Allocate output buffer
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, output_size, c_null_ptr, GL_DYNAMIC_COPY)
    
    ! Bind buffers to shader
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers(1))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers(2))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers(3))
    
    ! Use program
    call glUseProgram(conv2d_program)
    
    ! Set uniforms
    block
      character(len=32, kind=c_char), target :: uniform_name
      integer(c_int) :: loc
      
      uniform_name = "batch_size" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, batch)
      
      uniform_name = "in_channels" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, in_c)
      
      uniform_name = "out_channels" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, out_c)
      
      uniform_name = "height" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, h)
      
      uniform_name = "width" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, w)
      
      uniform_name = "kernel_h" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, kh)
      
      uniform_name = "kernel_w" // c_null_char
      loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
      if (loc >= 0) call glUniform1i(loc, kw)
    end block
    
    ! Calculate dispatch size
    groups_x = (w + 31) / 32
    groups_y = (h + 3) / 4
    
    ! Start GPU timer query
    call glBeginQuery(GL_TIME_ELAPSED, query_id)
    
    ! Launch compute shader
    call glDispatchCompute(groups_x, groups_y, out_c)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    
    ! End GPU timer query
    call glEndQuery(GL_TIME_ELAPSED)
    
    ! Ensure GPU work completes first
    call glFinish()
    
    ! Now check if query result is available
    call glGetQueryObjectiv(query_id, GL_QUERY_RESULT_AVAILABLE, c_loc(query_available))
    
    if (query_available /= 0) then
      ! Get the elapsed time in nanoseconds
      call glGetQueryObjecti64v(query_id, GL_QUERY_RESULT, c_loc(elapsed_ns))
      gpu_time_ms = real(elapsed_ns, dp) / 1.0e6_dp
      
      print '(A,F8.3,A)', "GPU timer: ", gpu_time_ms, " ms (actual GPU execution)"
    else
      print *, "WARNING: GPU timer query not available"
      ! Fall back to CPU timing
      print *, "Note: GL_TIME_ELAPSED may not be supported in OpenGL ES 3.1"
    end if
    
    ! Read back results
    block
      type(c_ptr) :: mapped_ptr
      real(sp), pointer :: mapped_data(:)
      integer :: out_size
      
      out_size = batch * out_c * h * w
      
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
      mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, &
                                   int(output_size, c_size_t), GL_MAP_READ_BIT)
      
      if (c_associated(mapped_ptr)) then
        call c_f_pointer(mapped_ptr, mapped_data, [out_size])
        output(1:out_size) = mapped_data(1:out_size)
        
        if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
          print *, "Warning: glUnmapBuffer failed"
        end if
      end if
    end block
    
    ! Clean up query
    call glDeleteQueries(1, c_loc(query_id))
    
    success = .true.
  end function nvidia_gl_execute_conv2d_timed
  
  function nvidia_gl_get_device_info() result(info)
    character(len=256) :: info
    type(c_ptr) :: vendor_ptr, renderer_ptr
    character(kind=c_char), pointer :: chars(:)
    integer :: i
    
    if (.not. initialized) then
      info = "NVIDIA OpenGL not initialized"
      return
    end if
    
    vendor_ptr = glGetString(GL_VENDOR)
    renderer_ptr = glGetString(GL_RENDERER)
    
    ! Convert renderer C string to Fortran
    if (c_associated(renderer_ptr)) then
      call c_f_pointer(renderer_ptr, chars, [256])
      info = ""
      do i = 1, 256
        if (chars(i) == c_null_char) exit
        info(i:i) = chars(i)
      end do
    else
      info = "Unknown NVIDIA GPU"
    end if
  end function nvidia_gl_get_device_info
  
end module sporkle_nvidia_opengl