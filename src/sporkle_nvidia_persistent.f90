module sporkle_nvidia_persistent
  ! Persistent buffer implementation for NVIDIA GPU
  ! Eliminates allocation overhead with pre-allocated ring buffers
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_opengl, only: nvidia_gl_init, nvidia_gl_shutdown, &
                                    nvidia_gl_get_device_info, &
                                    GL_SHADER_STORAGE_BUFFER, GL_DYNAMIC_COPY, &
                                    GL_MAP_READ_BIT, GL_SHADER_STORAGE_BARRIER_BIT, &
                                    glGenBuffers, glBindBuffer, glBufferData, &
                                    glBindBufferBase, glUseProgram, glUniform1i, &
                                    glGetUniformLocation, glDispatchCompute, &
                                    glMemoryBarrier, glFinish, glMapBufferRange, &
                                    glUnmapBuffer, conv2d_program
  implicit none
  
  private
  public :: persistent_init, persistent_shutdown
  public :: persistent_conv2d
  
  ! Missing OpenGL interfaces we need
  interface
    subroutine glBufferSubData(target, offset, size, data) bind(C, name='glBufferSubData')
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: offset, size
      type(c_ptr), value :: data
    end subroutine
    
    subroutine glDeleteBuffers(n, buffers) bind(C, name='glDeleteBuffers')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: buffers
    end subroutine
  end interface
  
  ! Ring buffer configuration
  integer, parameter :: RING_SIZE = 4  ! 4-deep ring buffer
  
  type :: buffer_set
    integer(c_int) :: input_buffer
    integer(c_int) :: kernel_buffer  
    integer(c_int) :: output_buffer
    logical :: in_use
  end type
  
  ! Persistent state
  type(buffer_set) :: ring_buffers(RING_SIZE)
  integer :: current_slot = 1
  logical :: initialized = .false.
  
  ! Maximum sizes we'll support (reduced for testing)
  integer, parameter :: MAX_BATCH = 4
  integer, parameter :: MAX_CHANNELS = 512
  integer, parameter :: MAX_HEIGHT = 256
  integer, parameter :: MAX_WIDTH = 256
  integer, parameter :: MAX_KERNEL_SIZE = 5
  
contains

  function persistent_init() result(success)
    logical :: success
    integer :: i
    integer(c_size_t) :: input_size, kernel_size, output_size
    
    success = .false.
    
    print *, "DEBUG: Starting persistent_init"
    
    ! Initialize OpenGL if needed
    if (.not. nvidia_gl_init()) then
      print *, "ERROR: Failed to initialize OpenGL"
      return
    end if
    
    print *, "DEBUG: OpenGL initialized"
    
    ! Calculate maximum buffer sizes
    input_size = MAX_BATCH * MAX_CHANNELS * MAX_HEIGHT * MAX_WIDTH * 4_c_size_t
    kernel_size = MAX_CHANNELS * MAX_CHANNELS * MAX_KERNEL_SIZE * MAX_KERNEL_SIZE * 4_c_size_t
    output_size = MAX_BATCH * MAX_CHANNELS * MAX_HEIGHT * MAX_WIDTH * 4_c_size_t
    
    print *, "=== Persistent Buffer Initialization ==="
    print '(A,I0,A)', "Creating ", RING_SIZE, "-deep ring buffer"
    print '(A,F6.1,A)', "Input buffer size: ", real(input_size) / (1024.0**3), " GB"
    print '(A,F6.1,A)', "Kernel buffer size: ", real(kernel_size) / (1024.0**3), " GB"
    print '(A,F6.1,A)', "Output buffer size: ", real(output_size) / (1024.0**3), " GB"
    print '(A,F6.1,A)', "Total GPU memory: ", &
      real(RING_SIZE * (input_size + kernel_size + output_size)) / (1024.0**3), " GB"
    
    ! Create persistent buffers for each ring slot
    do i = 1, RING_SIZE
      block
        use sporkle_nvidia_opengl
        integer(c_int), target :: buffers(3)
        
        ! Generate buffer IDs
        call glGenBuffers(3, c_loc(buffers))
        ring_buffers(i)%input_buffer = buffers(1)
        ring_buffers(i)%kernel_buffer = buffers(2)
        ring_buffers(i)%output_buffer = buffers(3)
        
        ! Allocate input buffer
        call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
        call glBufferData(GL_SHADER_STORAGE_BUFFER, input_size, c_null_ptr, GL_DYNAMIC_COPY)
        
        ! Allocate kernel buffer
        call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
        call glBufferData(GL_SHADER_STORAGE_BUFFER, kernel_size, c_null_ptr, GL_DYNAMIC_COPY)
        
        ! Allocate output buffer
        call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
        call glBufferData(GL_SHADER_STORAGE_BUFFER, output_size, c_null_ptr, GL_DYNAMIC_COPY)
        
        ring_buffers(i)%in_use = .false.
        
        print '(A,I0,A)', "  Slot ", i, " allocated"
      end block
    end do
    
    initialized = .true.
    success = .true.
    print *, "✅ Persistent buffers ready!"
    print *, ""
    
  end function persistent_init
  
  function persistent_conv2d(input, kernel, output, &
                            batch, in_c, out_c, h, w, kh, kw) result(time_ms)
    real(sp), intent(in), target :: input(*), kernel(*)
    real(sp), intent(out), target :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    real(dp) :: time_ms
    
    integer :: slot
    integer(c_size_t) :: input_size, kernel_size, output_size
    integer(c_int) :: groups_x, groups_y
    real(c_double) :: start_time, end_time
    
    if (.not. initialized) then
      print *, "ERROR: Persistent buffers not initialized"
      time_ms = -1.0_dp
      return
    end if
    
    ! Find next available slot
    slot = current_slot
    current_slot = mod(current_slot, RING_SIZE) + 1
    
    ! Calculate actual sizes
    input_size = batch * in_c * h * w * 4_c_size_t
    kernel_size = out_c * in_c * kh * kw * 4_c_size_t
    output_size = batch * out_c * h * w * 4_c_size_t
    
    ! Upload data to persistent buffers (using glBufferSubData for updates)
    block
      use sporkle_nvidia_opengl
      
      ! Update input buffer
      print *, "DEBUG: Updating input buffer, slot=", slot
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ring_buffers(slot)%input_buffer)
      call glBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, input_size, c_loc(input))
      
      ! Update kernel buffer  
      print *, "DEBUG: Updating kernel buffer"
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ring_buffers(slot)%kernel_buffer)
      call glBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, kernel_size, c_loc(kernel))
      
      ! Bind buffers for compute
      call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, ring_buffers(slot)%input_buffer)
      call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, ring_buffers(slot)%kernel_buffer)
      call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, ring_buffers(slot)%output_buffer)
      
      ! Use the compute program
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
      
      ! Time the execution
      call cpu_time(start_time)
      
      ! Launch compute
      call glDispatchCompute(groups_x, groups_y, out_c)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
      
      call cpu_time(end_time)
      
      ! Read back results
      block
        type(c_ptr) :: mapped_ptr
        real(sp), pointer :: mapped_data(:)
        integer :: out_elements
        
        out_elements = batch * out_c * h * w
        
        call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ring_buffers(slot)%output_buffer)
        mapped_ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, &
                                     output_size, GL_MAP_READ_BIT)
        
        if (c_associated(mapped_ptr)) then
          call c_f_pointer(mapped_ptr, mapped_data, [out_elements])
          output(1:out_elements) = mapped_data(1:out_elements)
          
          if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
            ! Ignore unmap errors for now
          end if
        end if
      end block
    end block
    
    time_ms = (end_time - start_time) * 1000.0_dp
    
  end function persistent_conv2d
  
  subroutine persistent_shutdown()
    integer :: i
    
    if (.not. initialized) return
    
    ! Clean up buffers
    do i = 1, RING_SIZE
      block
        use sporkle_nvidia_opengl
        integer(c_int), target :: buffers(3)
        
        buffers(1) = ring_buffers(i)%input_buffer
        buffers(2) = ring_buffers(i)%kernel_buffer
        buffers(3) = ring_buffers(i)%output_buffer
        
        call glDeleteBuffers(3, c_loc(buffers))
      end block
    end do
    
    initialized = .false.
    print *, "Persistent buffers shut down"
    
  end subroutine persistent_shutdown

end module sporkle_nvidia_persistent