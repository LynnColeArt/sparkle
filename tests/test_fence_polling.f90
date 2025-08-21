program test_fence_polling
  ! Test fence-based synchronization instead of glFinish()
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_opengl
  use flopcount
  use sporkle_types
  implicit none
  
  ! OpenGL fence interfaces
  interface
    function rdtsc_wrapper() bind(C, name='rdtsc_wrapper')
      import :: c_int64_t
      integer(c_int64_t) :: rdtsc_wrapper
    end function
    
    function glFenceSync(condition, flags) bind(C, name='glFenceSync')
      import :: c_int, c_ptr
      integer(c_int), value :: condition
      integer(c_int), value :: flags
      type(c_ptr) :: glFenceSync
    end function
    
    function glClientWaitSync(sync, flags, timeout) bind(C, name='glClientWaitSync')
      import :: c_int, c_int64_t, c_ptr
      type(c_ptr), value :: sync
      integer(c_int), value :: flags
      integer(c_int64_t), value :: timeout
      integer(c_int) :: glClientWaitSync
    end function
    
    subroutine glDeleteSync(sync) bind(C, name='glDeleteSync')
      import :: c_ptr
      type(c_ptr), value :: sync
    end subroutine
  end interface
  
  ! Fence constants (GL_SYNC_GPU_COMMANDS_COMPLETE already imported from module)
  integer(c_int), parameter :: GL_SYNC_FLUSH_COMMANDS_BIT = int(z'00000001', c_int)
  integer(c_int), parameter :: GL_ALREADY_SIGNALED = int(z'911A', c_int)
  integer(c_int), parameter :: GL_TIMEOUT_EXPIRED = int(z'911B', c_int)
  integer(c_int), parameter :: GL_CONDITION_SATISFIED = int(z'911C', c_int)
  integer(c_int), parameter :: GL_WAIT_FAILED = int(z'911D', c_int)
  
  ! Test configuration
  integer, parameter :: batch = 1
  integer, parameter :: height = 256, width = 256
  integer, parameter :: in_channels = 256, out_channels = 256
  integer, parameter :: kernel_h = 3, kernel_w = 3
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: h_out, w_out, i, run
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end, best_cycles
  integer(c_int64_t) :: fence_cycles, finish_cycles
  real(dp) :: cpu_ghz, time_ms, gflops, best_gflops
  real(dp) :: fence_time, finish_time
  integer(c_int), target :: buffers(3)
  type(c_ptr) :: fence
  integer(c_int) :: wait_result
  integer(c_int64_t), parameter :: timeout_ns = 1000000000_c_int64_t  ! 1 second
  
  print *, "=== Fence Polling vs glFinish() Comparison ==="
  print *, "Testing synchronization overhead reduction"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  cpu_ghz = 3.5_dp
  
  ! Calculate sizes
  h_out = height - kernel_h + 1
  w_out = width - kernel_w + 1
  
  ! Allocate and initialize arrays
  allocate(input(batch * in_channels * height * width))
  allocate(kernel(out_channels * in_channels * kernel_h * kernel_w))
  allocate(output(batch * out_channels * h_out * w_out))
  allocate(input_nhwc(batch * height * width * in_channels))
  allocate(output_nhwc(batch * h_out * w_out * out_channels))
  
  call random_number(input)
  call random_number(kernel)
  
  ! Convert to NHWC
  block
    integer :: n, c, h, w, nchw_idx, nhwc_idx
    do i = 1, batch * in_channels * height * width
      nchw_idx = i - 1
      w = mod(nchw_idx, width)
      nchw_idx = nchw_idx / width
      h = mod(nchw_idx, height)
      nchw_idx = nchw_idx / height
      c = mod(nchw_idx, in_channels)
      n = nchw_idx / in_channels
      
      nhwc_idx = ((n * height + h) * width + w) * in_channels + c + 1
      input_nhwc(nhwc_idx) = input(i)
    end do
  end block
  
  ! Calculate workload
  total_flops = conv2d_flops(int(batch, i64), int(in_channels, i64), &
                            int(out_channels, i64), int(h_out, i64), &
                            int(w_out, i64), int(kernel_h, i64), int(kernel_w, i64))
  
  ! Create buffers
  call glGenBuffers(3, c_loc(buffers))
  
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                   int(batch * height * width * in_channels * 4, c_size_t), &
                   c_loc(input_nhwc), GL_DYNAMIC_COPY)
  
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                   int(out_channels * in_channels * 9 * 4, c_size_t), &
                   c_loc(kernel), GL_DYNAMIC_COPY)
  
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                   int(batch * h_out * w_out * out_channels * 4, c_size_t), &
                   c_null_ptr, GL_DYNAMIC_COPY)
  
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers(1))
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers(2))
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers(3))
  
  call glUseProgram(conv2d_program)
  
  ! Set uniforms
  block
    character(len=32, kind=c_char), target :: uniform_name
    integer(c_int) :: loc
    
    uniform_name = "batch_size" // c_null_char
    loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
    if (loc >= 0) call glUniform1i(loc, batch)
    
    uniform_name = "height" // c_null_char
    loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
    if (loc >= 0) call glUniform1i(loc, height)
    
    uniform_name = "width" // c_null_char
    loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
    if (loc >= 0) call glUniform1i(loc, width)
    
    uniform_name = "in_channels" // c_null_char
    loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
    if (loc >= 0) call glUniform1i(loc, in_channels)
    
    uniform_name = "out_channels" // c_null_char
    loc = glGetUniformLocation(conv2d_program, c_loc(uniform_name))
    if (loc >= 0) call glUniform1i(loc, out_channels)
  end block
  
  block
    integer :: groups_x, groups_y, groups_z
    
    groups_x = (width + 31) / 32
    groups_y = (height + 31) / 32
    groups_z = batch
    
    ! Warm up
    print *, "Warming up GPU..."
    do i = 1, 5
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
    end do
    
    print *, ""
    print *, "=== METHOD 1: glFinish() (blocking) ==="
    
    finish_cycles = 0
    do run = 1, 10
      cycles_start = rdtsc_wrapper()
      
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()  ! Blocking wait
      
      cycles_end = rdtsc_wrapper()
      finish_cycles = finish_cycles + (cycles_end - cycles_start)
      
      time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
      gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
      
      print '(A,I2,A,F8.3,A,F10.1,A)', &
        "Run ", run, ": ", time_ms, " ms, ", gflops, " GFLOPS"
    end do
    
    finish_time = real(finish_cycles, dp) / (cpu_ghz * 1.0e6_dp * 10.0_dp)
    
    print *, ""
    print *, "=== METHOD 2: Fence polling (non-blocking) ==="
    
    fence_cycles = 0
    do run = 1, 10
      cycles_start = rdtsc_wrapper()
      
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      
      ! Create fence instead of glFinish
      fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0)
      
      ! Poll fence with minimal timeout
      wait_result = glClientWaitSync(fence, GL_SYNC_FLUSH_COMMANDS_BIT, timeout_ns)
      
      call glDeleteSync(fence)
      
      cycles_end = rdtsc_wrapper()
      fence_cycles = fence_cycles + (cycles_end - cycles_start)
      
      time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
      gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
      
      print '(A,I2,A,F8.3,A,F10.1,A)', &
        "Run ", run, ": ", time_ms, " ms, ", gflops, " GFLOPS"
      
      if (wait_result == GL_ALREADY_SIGNALED) then
        print *, "      (Fence already signaled - minimal overhead)"
      else if (wait_result == GL_CONDITION_SATISFIED) then
        print *, "      (Fence wait completed)"
      else if (wait_result == GL_TIMEOUT_EXPIRED) then
        print *, "      WARNING: Fence timeout!"
      end if
    end do
    
    fence_time = real(fence_cycles, dp) / (cpu_ghz * 1.0e6_dp * 10.0_dp)
  end block
  
  ! Results comparison
  print *, ""
  print *, "=== SYNCHRONIZATION OVERHEAD COMPARISON ==="
  print '(A,F8.3,A)', "Average glFinish() time: ", finish_time, " ms"
  print '(A,F8.3,A)', "Average fence poll time: ", fence_time, " ms"
  
  if (fence_time < finish_time) then
    print '(A,F5.2,A)', "Fence polling is ", finish_time / fence_time, "× faster!"
    print '(A,F8.3,A)', "Overhead reduction: ", (finish_time - fence_time), " ms per call"
  else
    print *, "No improvement (may need different fence strategy)"
  end if
  
  print *, ""
  print *, "Peak GFLOPS achievable:"
  print '(A,F10.1,A)', "  With glFinish: ", &
    real(total_flops, dp) / (finish_time * 1.0e6_dp), " GFLOPS"
  print '(A,F10.1,A)', "  With fence:    ", &
    real(total_flops, dp) / (fence_time * 1.0e6_dp), " GFLOPS"
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  deallocate(input_nhwc, output_nhwc)
  
end program test_fence_polling