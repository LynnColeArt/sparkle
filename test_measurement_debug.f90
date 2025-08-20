program test_measurement_debug
  ! Mini's Diagnostic #1: Fix measurement and hidden stalls
  ! Proper GPU timing vs CPU timing comparison
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_opengl
  use flopcount
  use sporkle_types
  implicit none
  
  ! RDTSC interface
  interface
    function rdtsc_wrapper() bind(C, name='rdtsc_wrapper')
      import :: c_int64_t
      integer(c_int64_t) :: rdtsc_wrapper
    end function
  end interface
  
  ! Test configuration
  integer, parameter :: batch = 1
  integer, parameter :: height = 256, width = 256
  integer, parameter :: in_channels = 256, out_channels = 256
  integer, parameter :: kernel_h = 3, kernel_w = 3
  integer, parameter :: num_query_slots = 8  ! Ring buffer for queries
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: h_out, w_out, i, run, current_slot
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end
  real(dp) :: cpu_ghz, cpu_time_ms, gpu_time_ms, cpu_gflops, gpu_gflops
  integer(c_int), target :: buffers(3)
  integer(c_int), target :: query_objects(num_query_slots)
  integer(c_int), target :: query_available, query_result_low, query_result_high
  integer(c_int64_t), target :: gpu_time_ns
  logical :: queries_supported
  
  print *, "================================================================"
  print *, "🔍 Mini's Diagnostic #1: Measurement & Hidden Stalls"
  print *, "================================================================"
  print *, ""
  print *, "Testing for measurement gotchas:"
  print *, "  ❌ CPU vs GPU timing discrepancies"
  print *, "  ❌ Query slot stalls (reading same slot you just wrote)"
  print *, "  ❌ Accidental synchronization points"
  print *, "  ❌ Debug context overhead"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  cpu_ghz = 3.5_dp
  
  ! Check if timer queries are supported
  call glGenQueries(num_query_slots, c_loc(query_objects))
  queries_supported = (query_objects(1) /= 0)
  
  if (queries_supported) then
    print *, "✅ GL_TIME_ELAPSED queries supported"
  else
    print *, "⚠️  GL_TIME_ELAPSED not supported, using CPU timing only"
  end if
  
  ! Calculate sizes and setup
  h_out = height - kernel_h + 1
  w_out = width - kernel_w + 1
  
  allocate(input(batch * in_channels * height * width))
  allocate(kernel(out_channels * in_channels * kernel_h * kernel_w))
  allocate(output(batch * out_channels * h_out * w_out))
  allocate(input_nhwc(batch * height * width * in_channels))
  allocate(output_nhwc(batch * h_out * w_out * out_channels))
  
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
  
  total_flops = conv2d_flops(int(batch, i64), int(in_channels, i64), &
                            int(out_channels, i64), int(h_out, i64), &
                            int(w_out, i64), int(kernel_h, i64), int(kernel_w, i64))
  
  print '(A,F10.3,A)', "Workload: ", real(total_flops) / 1.0e9, " GFLOPs"
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
  call set_uniforms()
  
  print *, "🏁 TIMING COMPARISON TESTS"
  print *, "=========================="
  
  block
    integer :: groups_x, groups_y, groups_z
    
    groups_x = (width + 31) / 32
    groups_y = (height + 31) / 32
    groups_z = batch
    
    print '(A,I0,A,I0)', "Dispatch: ", groups_x, "×", groups_y, " workgroups"
    print *, ""
    
    ! === TEST 1: CPU Timing (RDTSC) ===
    print *, "📊 TEST 1: CPU Timing (RDTSC + glFinish)"
    print *, "========================================="
    print *, "This shows total time including GPU→CPU sync overhead"
    print *, ""
    
    ! Warm up
    do i = 1, 5
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
    end do
    
    do run = 1, 5
      cycles_start = rdtsc_wrapper()
      
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()  ! BLOCKING - includes sync overhead
      
      cycles_end = rdtsc_wrapper()
      
      cpu_time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
      cpu_gflops = real(total_flops, dp) / (cpu_time_ms * 1.0e6_dp)
      
      print '(A,I2,A,F8.3,A,F10.1,A)', &
        "  Run ", run, ": ", cpu_time_ms, " ms → ", cpu_gflops, " GFLOPS (CPU+sync)"
    end do
    
    print *, ""
    
    ! === TEST 2: GPU Timer Queries (if supported) ===
    if (queries_supported) then
      print *, "⚡ TEST 2: GPU Timer Queries (GL_TIME_ELAPSED)"
      print *, "=============================================="
      print *, "This shows pure GPU execution time (no sync overhead)"
      print *, ""
      
      current_slot = 1
      
      ! Warm up and fill query slots
      do i = 1, min(5, num_query_slots)
        call glBeginQuery(GL_TIME_ELAPSED, query_objects(i))
        call glDispatchCompute(groups_x, groups_y, groups_z)
        call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
        call glEndQuery(GL_TIME_ELAPSED)
      end do
      
      ! Wait for first few queries to complete
      call glFinish()
      
      ! Performance runs with proper query ring buffer
      do run = 1, 5
        current_slot = mod(run - 1, num_query_slots) + 1
        
        ! Check if this slot is available (don't read same slot you just wrote!)
        query_available = 0
        call glGetQueryObjectiv(query_objects(current_slot), GL_QUERY_RESULT_AVAILABLE, &
                               c_loc(query_available))
        
        if (query_available /= 0) then
          ! Read the result from this slot (older dispatch)
          call glGetQueryObjecti64v(query_objects(current_slot), GL_QUERY_RESULT, &
                                   c_loc(gpu_time_ns))
          
          gpu_time_ms = real(gpu_time_ns, dp) / 1.0e6_dp
          gpu_gflops = real(total_flops, dp) / (gpu_time_ms * 1.0e6_dp)
          
          print '(A,I2,A,F8.3,A,F10.1,A)', &
            "  Run ", run, ": ", gpu_time_ms, " ms → ", gpu_gflops, " GFLOPS (GPU pure)"
        else
          print '(A,I2,A)', "  Run ", run, ": Query not ready (would stall)"
        end if
        
        ! Start new query in this slot
        call glBeginQuery(GL_TIME_ELAPSED, query_objects(current_slot))
        call glDispatchCompute(groups_x, groups_y, groups_z)
        call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
        call glEndQuery(GL_TIME_ELAPSED)
      end do
      
      ! Finish remaining queries
      call glFinish()
      
      ! Read any remaining results
      do i = 1, num_query_slots
        query_available = 0
        call glGetQueryObjectiv(query_objects(i), GL_QUERY_RESULT_AVAILABLE, &
                               c_loc(query_available))
        if (query_available /= 0) then
          call glGetQueryObjecti64v(query_objects(i), GL_QUERY_RESULT, &
                                   c_loc(gpu_time_ns))
          gpu_time_ms = real(gpu_time_ns, dp) / 1.0e6_dp
          gpu_gflops = real(total_flops, dp) / (gpu_time_ms * 1.0e6_dp)
          print '(A,I2,A,F8.3,A,F10.1,A)', &
            "  Final slot ", i, ": ", gpu_time_ms, " ms → ", gpu_gflops, " GFLOPS"
        end if
      end do
    end if
    
    print *, ""
    
    ! === TEST 3: No-op kernel timing ===
    print *, "🔧 TEST 3: Dispatch Overhead (Minimal Kernel)"
    print *, "============================================="
    print *, "Testing pure dispatch/queue overhead"
    print *, ""
    
    ! Time empty dispatches to measure overhead
    cycles_start = rdtsc_wrapper()
    
    do i = 1, 10
      call glDispatchCompute(1, 1, 1)  ! Minimal dispatch
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    end do
    call glFinish()
    
    cycles_end = rdtsc_wrapper()
    
    cpu_time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
    print '(A,F8.3,A)', "  10 minimal dispatches: ", cpu_time_ms, " ms total"
    print '(A,F8.3,A)', "  Overhead per dispatch: ", cpu_time_ms / 10.0, " ms"
    
    if (cpu_time_ms / 10.0 > 1.0) then
      print *, "  ⚠️  HIGH DISPATCH OVERHEAD detected!"
      print *, "     Consider async pipeline or batch optimization"
    else
      print *, "  ✅ Dispatch overhead acceptable"
    end if
  end block
  
  print *, ""
  print *, "================================================================"
  print *, "🔍 MEASUREMENT DIAGNOSTIC SUMMARY"
  print *, "================================================================"
  print *, ""
  print *, "Key Issues to Check:"
  print *, "  🔸 CPU vs GPU timing difference = sync overhead"
  print *, "  🔸 Query slot stalls = reading same slot you just wrote"
  print *, "  🔸 High dispatch overhead = driver/queue issues"
  print *, "  🔸 Missing GPU queries = fallback to CPU timing"
  print *, ""
  print *, "Next: Run Mini's Diagnostic #2 (Residency Test)"
  
  ! Clean up
  if (queries_supported) then
    ! Note: glDeleteQueries not available in our interface, skip cleanup
    ! Objects will be freed when context is destroyed
  end if
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  deallocate(input_nhwc, output_nhwc)

contains

  subroutine set_uniforms()
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
  end subroutine set_uniforms

end program test_measurement_debug