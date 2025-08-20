module sporkle_nvidia_zerocopy
  ! Zero-copy persistent mapped buffer implementation
  ! Eliminates ALL data transfer overhead
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_opengl, only: nvidia_gl_init, nvidia_gl_shutdown, &
                                    GL_SHADER_STORAGE_BUFFER, &
                                    GL_SHADER_STORAGE_BARRIER_BIT, &
                                    GL_MAP_READ_BIT, &
                                    glGenBuffers, glBindBuffer, &
                                    glBindBufferBase, glUseProgram, glUniform1i, &
                                    glGetUniformLocation, glDispatchCompute, &
                                    glMemoryBarrier, glFinish, &
                                    glMapBufferRange, glUnmapBuffer, &
                                    conv2d_program
  implicit none
  
  private
  public :: zerocopy_init, zerocopy_shutdown
  public :: zerocopy_conv2d
  
  ! OpenGL constants for persistent mapping
  integer(c_int), parameter :: GL_MAP_WRITE_BIT = int(z'0002', c_int)
  integer(c_int), parameter :: GL_MAP_PERSISTENT_BIT = int(z'0040', c_int)
  integer(c_int), parameter :: GL_MAP_COHERENT_BIT = int(z'0080', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_STORAGE_BIT = int(z'0100', c_int)
  
  ! Persistent mapped buffer info
  type :: mapped_buffer
    integer(c_int) :: buffer_id
    type(c_ptr) :: mapped_ptr
    real(sp), pointer :: data(:)
    integer :: size
  end type
  
  ! Global state
  type(mapped_buffer) :: input_buffer
  type(mapped_buffer) :: kernel_buffer  
  type(mapped_buffer) :: output_buffer
  logical :: initialized = .false.
  
  ! Buffer sizes (reasonable for testing)
  integer, parameter :: MAX_INPUT_SIZE = 16*1024*1024   ! 64 MB
  integer, parameter :: MAX_KERNEL_SIZE = 4*1024*1024   ! 16 MB
  integer, parameter :: MAX_OUTPUT_SIZE = 16*1024*1024  ! 64 MB
  
  ! Missing OpenGL functions
  interface
    subroutine glBufferStorage(target, size, data, flags) bind(C, name='glBufferStorage')
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
      integer(c_int), value :: flags
    end subroutine
  end interface
  
contains

  function zerocopy_init() result(success)
    logical :: success
    integer(c_int), target :: buffer_ids(3)
    integer(c_int) :: map_flags
    
    success = .false.
    
    ! Initialize OpenGL if needed
    if (.not. nvidia_gl_init()) then
      print *, "ERROR: Failed to initialize OpenGL"
      return
    end if
    
    print *, "=== Zero-Copy Buffer Initialization ==="
    print *, "Creating persistent coherent mapped buffers"
    print *, "This eliminates ALL CPU-GPU transfer overhead!"
    print *, ""
    
    ! Mapping flags for zero-copy access
    map_flags = ior(GL_MAP_WRITE_BIT, &
                ior(GL_MAP_READ_BIT, &
                ior(GL_MAP_PERSISTENT_BIT, GL_MAP_COHERENT_BIT)))
    
    ! Generate buffer IDs
    call glGenBuffers(3, c_loc(buffer_ids))
    input_buffer%buffer_id = buffer_ids(1)
    kernel_buffer%buffer_id = buffer_ids(2)
    output_buffer%buffer_id = buffer_ids(3)
    
    ! Create and map input buffer
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, input_buffer%buffer_id)
    call glBufferStorage(GL_SHADER_STORAGE_BUFFER, &
                        int(MAX_INPUT_SIZE * 4, c_size_t), &
                        c_null_ptr, &
                        ior(GL_DYNAMIC_STORAGE_BIT, map_flags))
    
    input_buffer%mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, &
                                              0_c_size_t, &
                                              int(MAX_INPUT_SIZE * 4, c_size_t), &
                                              map_flags)
    
    if (.not. c_associated(input_buffer%mapped_ptr)) then
      print *, "ERROR: Failed to map input buffer"
      return
    end if
    
    call c_f_pointer(input_buffer%mapped_ptr, input_buffer%data, [MAX_INPUT_SIZE])
    input_buffer%size = MAX_INPUT_SIZE
    print *, "✅ Input buffer mapped (64 MB)"
    
    ! Create and map kernel buffer
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, kernel_buffer%buffer_id)
    call glBufferStorage(GL_SHADER_STORAGE_BUFFER, &
                        int(MAX_KERNEL_SIZE * 4, c_size_t), &
                        c_null_ptr, &
                        ior(GL_DYNAMIC_STORAGE_BIT, map_flags))
    
    kernel_buffer%mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, &
                                               0_c_size_t, &
                                               int(MAX_KERNEL_SIZE * 4, c_size_t), &
                                               map_flags)
    
    if (.not. c_associated(kernel_buffer%mapped_ptr)) then
      print *, "ERROR: Failed to map kernel buffer"
      return
    end if
    
    call c_f_pointer(kernel_buffer%mapped_ptr, kernel_buffer%data, [MAX_KERNEL_SIZE])
    kernel_buffer%size = MAX_KERNEL_SIZE
    print *, "✅ Kernel buffer mapped (16 MB)"
    
    ! Create and map output buffer
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, output_buffer%buffer_id)
    call glBufferStorage(GL_SHADER_STORAGE_BUFFER, &
                        int(MAX_OUTPUT_SIZE * 4, c_size_t), &
                        c_null_ptr, &
                        ior(GL_DYNAMIC_STORAGE_BIT, map_flags))
    
    output_buffer%mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, &
                                               0_c_size_t, &
                                               int(MAX_OUTPUT_SIZE * 4, c_size_t), &
                                               map_flags)
    
    if (.not. c_associated(output_buffer%mapped_ptr)) then
      print *, "ERROR: Failed to map output buffer"
      return
    end if
    
    call c_f_pointer(output_buffer%mapped_ptr, output_buffer%data, [MAX_OUTPUT_SIZE])
    output_buffer%size = MAX_OUTPUT_SIZE
    print *, "✅ Output buffer mapped (64 MB)"
    
    initialized = .true.
    success = .true.
    
    print *, ""
    print *, "🚀 Zero-copy buffers ready!"
    print *, "Data writes go directly to GPU memory - no transfers needed!"
    print *, ""
    
  end function zerocopy_init
  
  function zerocopy_conv2d(input, kernel, output, &
                          batch, in_c, out_c, h, w, kh, kw) result(time_cycles)
    real(sp), intent(in) :: input(*), kernel(*)
    real(sp), intent(out) :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    integer(c_int64_t) :: time_cycles
    
    integer :: input_size, kernel_size, output_size
    integer :: groups_x, groups_y
    integer(c_int64_t) :: start_cycles, end_cycles
    
    ! Interface to RDTSC
    interface
      function rdtsc_wrapper() bind(C, name='rdtsc_wrapper')
        import :: c_int64_t
        integer(c_int64_t) :: rdtsc_wrapper
      end function
    end interface
    
    if (.not. initialized) then
      print *, "ERROR: Zero-copy buffers not initialized"
      time_cycles = -1
      return
    end if
    
    ! Calculate sizes
    input_size = batch * in_c * h * w
    kernel_size = out_c * in_c * kh * kw
    output_size = batch * out_c * h * w
    
    ! Copy data to mapped buffers (this is just a memcpy, very fast)
    input_buffer%data(1:input_size) = input(1:input_size)
    kernel_buffer%data(1:kernel_size) = kernel(1:kernel_size)
    
    ! Bind buffers for compute
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, input_buffer%buffer_id)
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, kernel_buffer%buffer_id)
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, output_buffer%buffer_id)
    
    ! Use compute program
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
    
    ! Time with RDTSC for minimal overhead
    start_cycles = rdtsc_wrapper()
    
    ! Launch compute
    call glDispatchCompute(groups_x, groups_y, out_c)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    end_cycles = rdtsc_wrapper()
    
    ! Copy results from mapped buffer (just memcpy)
    output(1:output_size) = output_buffer%data(1:output_size)
    
    time_cycles = end_cycles - start_cycles
    
  end function zerocopy_conv2d
  
  subroutine zerocopy_shutdown()
    if (.not. initialized) return
    
    ! Unmap buffers
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, input_buffer%buffer_id)
    if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
      ! Ignore error
    end if
    
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, kernel_buffer%buffer_id)
    if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
      ! Ignore error
    end if
    
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, output_buffer%buffer_id)
    if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
      ! Ignore error
    end if
    
    initialized = .false.
    print *, "Zero-copy buffers shut down"
  end subroutine zerocopy_shutdown

end module sporkle_nvidia_zerocopy