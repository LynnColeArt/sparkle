program test_nvidia_opengl
  ! Test NVIDIA GPU via OpenGL compute shaders
  ! This should work immediately since NVIDIA has excellent OpenGL support
  
  use iso_c_binding
  implicit none
  
  ! OpenGL/EGL interfaces
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
    
    function eglChooseConfig(display, attrib_list, configs, config_size, num_config) &
        bind(C, name='eglChooseConfig')
      import :: c_ptr, c_int
      type(c_ptr), value :: display
      type(c_ptr), value :: attrib_list
      type(c_ptr), value :: configs
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
    
    subroutine glGetIntegerv(pname, params) bind(C, name='glGetIntegerv')
      import :: c_int, c_ptr
      integer(c_int), value :: pname
      type(c_ptr), value :: params
    end subroutine
  end interface
  
  ! Constants
  integer(c_int), parameter :: EGL_DEFAULT_DISPLAY = 0
  integer(c_int), parameter :: EGL_NO_SURFACE = 0
  integer(c_int), parameter :: EGL_NO_CONTEXT = 0
  integer(c_int), parameter :: EGL_OPENGL_ES_API = int(z'30A0', c_int)
  integer(c_int), parameter :: EGL_OPENGL_API = int(z'30A2', c_int)
  integer(c_int), parameter :: EGL_RENDERABLE_TYPE = int(z'3040', c_int)
  integer(c_int), parameter :: EGL_OPENGL_BIT = int(z'0008', c_int)
  integer(c_int), parameter :: EGL_CONTEXT_MAJOR_VERSION = int(z'3098', c_int)
  integer(c_int), parameter :: EGL_CONTEXT_MINOR_VERSION = int(z'30FB', c_int)
  integer(c_int), parameter :: EGL_NONE = int(z'3038', c_int)
  
  integer(c_int), parameter :: GL_VENDOR = int(z'1F00', c_int)
  integer(c_int), parameter :: GL_RENDERER = int(z'1F01', c_int)
  integer(c_int), parameter :: GL_VERSION = int(z'1F02', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_WORK_GROUP_COUNT = int(z'91BE', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_WORK_GROUP_SIZE = int(z'91BF', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_SHARED_MEMORY_SIZE = int(z'8262', c_int)
  
  ! Variables
  type(c_ptr) :: display, context
  type(c_ptr), target :: config_ptr
  integer(c_int), target :: major, minor, num_configs
  integer(c_int), target :: config_attribs(5), context_attribs(7)
  integer(c_int), target :: work_group_count(3), work_group_size(3), shared_mem_size
  type(c_ptr) :: vendor_str, renderer_str, version_str
  character(len=256) :: vendor, renderer, version
  integer :: i, ret
  
  print *, "========================================="
  print *, "Sporkle NVIDIA OpenGL Compute Test"
  print *, "========================================="
  print *, ""
  
  ! Initialize EGL
  print *, "Initializing EGL..."
  display = eglGetDisplay(c_null_ptr)
  if (.not. c_associated(display)) then
    print *, "Failed to get EGL display"
    stop 1
  end if
  
  ret = eglInitialize(display, c_loc(major), c_loc(minor))
  if (ret == 0) then
    print *, "Failed to initialize EGL"
    stop 1
  end if
  print *, "EGL initialized: version", major, ".", minor
  
  ! Choose config
  config_attribs = [EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT, EGL_NONE, 0, 0]
  ret = eglChooseConfig(display, c_loc(config_attribs), c_loc(config_ptr), 1, c_loc(num_configs))
  if (ret == 0 .or. num_configs == 0) then
    print *, "Failed to choose EGL config"
    stop 1
  end if
  
  ! Create context with OpenGL 4.6 (compute shaders)
  context_attribs = [EGL_CONTEXT_MAJOR_VERSION, 4, &
                      EGL_CONTEXT_MINOR_VERSION, 6, &
                      EGL_NONE, 0, 0]
  
  context = eglCreateContext(display, config_ptr, c_null_ptr, c_loc(context_attribs))
  if (.not. c_associated(context)) then
    print *, "Failed to create OpenGL context"
    stop 1
  end if
  
  ! Make current
  ret = eglMakeCurrent(display, c_null_ptr, c_null_ptr, context)
  if (ret == 0) then
    print *, "Failed to make context current"
    stop 1
  end if
  
  print *, "OpenGL context created successfully!"
  print *, ""
  
  ! Get GPU information
  vendor_str = glGetString(GL_VENDOR)
  renderer_str = glGetString(GL_RENDERER)
  version_str = glGetString(GL_VERSION)
  
  call c_f_string(vendor_str, vendor)
  call c_f_string(renderer_str, renderer)
  call c_f_string(version_str, version)
  
  print *, "GPU Information:"
  print *, "  Vendor:  ", trim(vendor)
  print *, "  Renderer:", trim(renderer)
  print *, "  Version: ", trim(version)
  print *, ""
  
  ! Get compute capabilities
  print *, "Compute Capabilities:"
  
  do i = 0, 2
    call glGetIntegerv(GL_MAX_COMPUTE_WORK_GROUP_COUNT + i, c_loc(work_group_count(i+1)))
  end do
  print *, "  Max work group count:", work_group_count
  
  do i = 0, 2
    call glGetIntegerv(GL_MAX_COMPUTE_WORK_GROUP_SIZE + i, c_loc(work_group_size(i+1)))
  end do
  print *, "  Max work group size:", work_group_size
  
  call glGetIntegerv(GL_MAX_COMPUTE_SHARED_MEMORY_SIZE, c_loc(shared_mem_size))
  print *, "  Max shared memory:", shared_mem_size, "bytes"
  
  print *, ""
  print *, "========================================="
  print *, "NVIDIA GPU ready for compute shaders!"
  print *, "A4500: 5,888 CUDA cores await!"
  print *, "========================================="
  
contains
  
  subroutine c_f_string(c_str, f_str)
    type(c_ptr), intent(in) :: c_str
    character(len=*), intent(out) :: f_str
    character(kind=c_char), pointer :: chars(:)
    integer :: i
    
    if (.not. c_associated(c_str)) then
      f_str = ""
      return
    end if
    
    call c_f_pointer(c_str, chars, [256])
    f_str = ""
    do i = 1, 256
      if (chars(i) == c_null_char) exit
      f_str(i:i) = chars(i)
    end do
  end subroutine c_f_string
  
end program test_nvidia_opengl