program test_async_pattern
  ! Test async pipeline pattern to hide dispatch overhead
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_opengl
  use flopcount
  use sporkle_types
  implicit none
  
  ! RDTSC and fence interfaces
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
  
  ! Fence constants
  integer(c_int), parameter :: GL_SYNC_FLUSH_COMMANDS_BIT = int(z'00000001', c_int)
  integer(c_int), parameter :: GL_ALREADY_SIGNALED = int(z'911A', c_int)
  integer(c_int), parameter :: GL_CONDITION_SATISFIED = int(z'911C', c_int)
  integer(c_int), parameter :: GL_TIMEOUT_EXPIRED = int(z'911B', c_int)
  
  ! Async pipeline configuration
  integer, parameter :: pipeline_depth = 4
  integer, parameter :: batch_size = 8  ! Process 8 "batches" through pipeline
  
  ! Test configuration - optimal 256×256
  integer, parameter :: batch = 1
  integer, parameter :: height = 256, width = 256
  integer, parameter :: in_channels = 256, out_channels = 256
  integer, parameter :: kernel_h = 3, kernel_w = 3
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: h_out, w_out, i, run, pipe_stage
  integer(i64) :: total_flops, total_flops_all
  integer(c_int64_t) :: cycles_start, cycles_end
  integer(c_int64_t) :: sync_cycles, async_cycles
  real(dp) :: cpu_ghz, time_ms, gflops
  real(dp) :: sync_time, async_time, speedup
  integer(c_int), target :: buffers(3)
  
  ! Pipeline state
  type(c_ptr) :: fences(pipeline_depth)
  integer :: fence_idx
  
  print *, "=== Async Pipeline Pattern Test ==="
  print *, "Testing GPU command queue overlap for maximum throughput"
  print *, ""
  print '(A,I0)', "Pipeline depth: ", pipeline_depth
  print '(A,I0)', "Batch count: ", batch_size
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
  
  ! Allocate arrays
  allocate(input(batch * in_channels * height * width))
  allocate(kernel(out_channels * in_channels * kernel_h * kernel_w))
  allocate(output(batch * out_channels * h_out * w_out))
  allocate(input_nhwc(batch * height * width * in_channels))
  allocate(output_nhwc(batch * h_out * w_out * out_channels))
  
  ! Initialize data
  call random_number(input)
  call random_number(kernel)
  input = input - 0.5
  kernel = kernel * 0.1
  
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
  
  ! Calculate workload per dispatch and total
  total_flops = conv2d_flops(int(batch, i64), int(in_channels, i64), &
                            int(out_channels, i64), int(h_out, i64), &
                            int(w_out, i64), int(kernel_h, i64), int(kernel_w, i64))
  total_flops_all = total_flops * batch_size
  
  print '(A,F10.3,A)', "FLOPs per dispatch: ", real(total_flops) / 1.0e9, " GFLOPs"
  print '(A,F10.3,A)', "Total FLOPs: ", real(total_flops_all) / 1.0e9, " GFLOPs"
  print *, ""
  
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
    
    print '(A,I0,A,I0)', "Dispatch size: ", groups_x, "×", groups_y, " workgroups"
    print *, ""
    
    ! Warm up GPU
    print *, "Warming up GPU..."
    do i = 1, 5
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
    end do
    
    ! === METHOD 1: Synchronous (baseline) ===
    print *, ""
    print *, "=== SYNCHRONOUS BASELINE ==="
    print *, "Sequential dispatches with full synchronization"
    
    cycles_start = rdtsc_wrapper()
    
    do i = 1, batch_size
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()  ! Full sync each time
    end do
    
    cycles_end = rdtsc_wrapper()
    sync_cycles = cycles_end - cycles_start
    
    sync_time = real(sync_cycles, dp) / (cpu_ghz * 1.0e6_dp)
    gflops = real(total_flops_all, dp) / (sync_time * 1.0e6_dp)
    
    print '(A,F8.3,A)', "Total time: ", sync_time, " ms"
    print '(A,F8.3,A)', "Per dispatch: ", sync_time / batch_size, " ms"
    print '(A,F10.1,A)', "Performance: ", gflops, " GFLOPS"
    
    ! === METHOD 2: Async Pipeline ===
    print *, ""
    print *, "=== ASYNC PIPELINE ==="
    print *, "Overlapped dispatches with fence-based synchronization"
    
    ! Initialize fences
    do i = 1, pipeline_depth
      fences(i) = c_null_ptr
    end do
    fence_idx = 1
    
    cycles_start = rdtsc_wrapper()
    
    ! Fill the pipeline
    do i = 1, min(pipeline_depth, batch_size)
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      
      ! Create fence for this dispatch
      fences(fence_idx) = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0)
      fence_idx = mod(fence_idx, pipeline_depth) + 1
    end do
    
    ! Process remaining dispatches with overlap
    do i = pipeline_depth + 1, batch_size
      ! Wait for oldest fence to complete (oldest slot)
      if (c_associated(fences(fence_idx))) then
        do
          if (glClientWaitSync(fences(fence_idx), GL_SYNC_FLUSH_COMMANDS_BIT, 0_c_int64_t) &
              /= GL_TIMEOUT_EXPIRED) exit
          ! Could do CPU work here while waiting
        end do
        call glDeleteSync(fences(fence_idx))
      end if
      
      ! Submit new dispatch to this slot
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      fences(fence_idx) = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0)
      fence_idx = mod(fence_idx, pipeline_depth) + 1
    end do
    
    ! Wait for all remaining fences
    do i = 1, pipeline_depth
      if (c_associated(fences(i))) then
        do
          if (glClientWaitSync(fences(i), GL_SYNC_FLUSH_COMMANDS_BIT, 1000000000_c_int64_t) &
              /= GL_TIMEOUT_EXPIRED) exit
        end do
        call glDeleteSync(fences(i))
      end if
    end do
    
    cycles_end = rdtsc_wrapper()
    async_cycles = cycles_end - cycles_start
    
    async_time = real(async_cycles, dp) / (cpu_ghz * 1.0e6_dp)
    gflops = real(total_flops_all, dp) / (async_time * 1.0e6_dp)
    
    print '(A,F8.3,A)', "Total time: ", async_time, " ms"
    print '(A,F8.3,A)', "Per dispatch: ", async_time / batch_size, " ms"
    print '(A,F10.1,A)', "Performance: ", gflops, " GFLOPS"
  end block
  
  ! Analysis
  print *, ""
  print *, "=== PIPELINE ANALYSIS ==="
  
  speedup = sync_time / async_time
  print '(A,F5.2,A)', "Async speedup: ", speedup, "×"
  print '(A,F8.3,A)', "Overhead reduction: ", (sync_time - async_time), " ms"
  print '(A,F8.3,A)', "Per-dispatch saving: ", (sync_time - async_time) / batch_size, " ms"
  
  if (speedup > 1.1) then
    print *, ""
    print *, "🚀 PIPELINE WORKING!"
    print *, "Command queue overlap is hiding dispatch overhead"
  else
    print *, ""
    print *, "Limited improvement - kernel compute time dominates"
    print *, "Need larger batches or multiple buffers for better overlap"
  end if
  
  print *, ""
  print *, "Next optimizations:"
  print *, "1. Multiple buffer sets for true ping-pong"
  print *, "2. Larger workloads to amortize overhead"
  print *, "3. CPU/GPU work overlap during fence waits"
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  deallocate(input_nhwc, output_nhwc)
  
end program test_async_pattern