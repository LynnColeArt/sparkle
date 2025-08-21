program test_residency_debug
  ! Mini's Diagnostic #2: Residency & device-local compute test
  ! Check if we're falling into the "zero-copy made it slower" trap
  
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
  
  ! OpenGL constants  
  integer(c_int), parameter :: GL_STATIC_DRAW = int(z'88E4', c_int)
  
  ! Test configuration
  integer, parameter :: batch = 1
  integer, parameter :: height = 256, width = 256
  integer, parameter :: in_channels = 256, out_channels = 256
  integer, parameter :: kernel_h = 3, kernel_w = 3
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: h_out, w_out, i, run
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end
  real(dp) :: cpu_ghz, time_ms, gflops
  real(dp) :: run1_gflops, run2_gflops, speedup
  integer(c_int), target :: buffers(3)
  
  print *, "================================================================"
  print *, "🏠 Mini's Diagnostic #2: Residency & Device-Local Test"
  print *, "================================================================"
  print *, ""
  print *, "Testing the 'zero-copy made it slower' trap:"
  print *, "  🔸 Run #1: Upload + compute (may trigger migration)"
  print *, "  🔸 Run #2: Compute only (should be faster if device-local)"
  print *, "  🔸 No speedup = computing on host-visible memory!"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  cpu_ghz = 3.5_dp
  
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
  
  ! Test buffer usage patterns
  call glGenBuffers(3, c_loc(buffers))
  
  call glUseProgram(conv2d_program)
  call set_uniforms()
  
  block
    integer :: groups_x, groups_y, groups_z
    
    groups_x = (width + 31) / 32
    groups_y = (height + 31) / 32
    groups_z = batch
    
    print *, "🔄 RESIDENCY TEST SEQUENCE"
    print *, "=========================="
    print *, ""
    
    ! === RUN 1: Upload + Compute ===
    print *, "📤 RUN 1: Upload + Compute (potential migration)"
    print *, "-----------------------------------------------"
    
    ! Create and upload buffers (this may trigger migration)
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(batch * height * width * in_channels * 4, c_size_t), &
                     c_loc(input_nhwc), GL_DYNAMIC_COPY)  ! Upload
    
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(out_channels * in_channels * 9 * 4, c_size_t), &
                     c_loc(kernel), GL_DYNAMIC_COPY)  ! Upload
    
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(batch * h_out * w_out * out_channels * 4, c_size_t), &
                     c_null_ptr, GL_DYNAMIC_COPY)  ! Allocate
    
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers(1))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers(2))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers(3))
    
    ! Immediate dispatch (may include migration time)
    cycles_start = rdtsc_wrapper()
    
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    cycles_end = rdtsc_wrapper()
    
    time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
    run1_gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
    
    print '(A,F8.3,A,F10.1,A)', "  Upload+Compute: ", time_ms, " ms → ", run1_gflops, " GFLOPS"
    print *, ""
    
    ! === RUN 2: Compute Only ===
    print *, "⚡ RUN 2: Compute Only (no upload, pure compute)"
    print *, "----------------------------------------------"
    
    ! Dispatch same computation WITHOUT re-upload
    cycles_start = rdtsc_wrapper()
    
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    cycles_end = rdtsc_wrapper()
    
    time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
    run2_gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
    
    print '(A,F8.3,A,F10.1,A)', "  Compute only:   ", time_ms, " ms → ", run2_gflops, " GFLOPS"
    print *, ""
    
    ! === ANALYSIS ===
    print *, "📊 RESIDENCY ANALYSIS"
    print *, "===================="
    
    speedup = run2_gflops / run1_gflops
    
    print '(A,F10.1,A)', "Run #1 (upload+compute): ", run1_gflops, " GFLOPS"
    print '(A,F10.1,A)', "Run #2 (compute only):   ", run2_gflops, " GFLOPS"
    print '(A,F5.2,A)', "Speedup: ", speedup, "×"
    print *, ""
    
    if (speedup > 1.2) then
      print *, "✅ DEVICE-LOCAL CONFIRMED!"
      print *, "   Run #2 is significantly faster → data migrated to VRAM"
      print *, "   Buffers are truly device-local for compute"
    else if (speedup > 1.05) then
      print *, "⚠️  MINOR IMPROVEMENT"
      print *, "   Small speedup suggests some device-local behavior"
      print *, "   But may not be fully optimized"
    else
      print *, "❌ HOST-VISIBLE MEMORY TRAP!"
      print *, "   No speedup → computing on host-visible memory"
      print *, "   Buffers are in PCIe-accessible RAM, not VRAM"
      print *, ""
      print *, "🔧 FIXES NEEDED:"
      print *, "   • Use GL_STATIC_DRAW instead of GL_DYNAMIC_COPY"
      print *, "   • Force device-local allocation hints"
      print *, "   • Check memory type queries"
    end if
    
    print *, ""
    
    ! === ADDITIONAL CHECKS ===
    print *, "🔍 ADDITIONAL RESIDENCY CHECKS"
    print *, "=============================="
    
    ! Test with different usage patterns
    print *, "Testing GL_STATIC_DRAW allocation..."
    
    ! Recreate buffers with STATIC_DRAW
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(batch * height * width * in_channels * 4, c_size_t), &
                     c_loc(input_nhwc), GL_STATIC_DRAW)  ! Different usage hint
    
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(out_channels * in_channels * 9 * 4, c_size_t), &
                     c_loc(kernel), GL_STATIC_DRAW)
    
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(batch * h_out * w_out * out_channels * 4, c_size_t), &
                     c_null_ptr, GL_STATIC_DRAW)
    
    ! Test with STATIC_DRAW
    cycles_start = rdtsc_wrapper()
    
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    cycles_end = rdtsc_wrapper()
    
    time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
    gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
    
    print '(A,F8.3,A,F10.1,A)', "  STATIC_DRAW:    ", time_ms, " ms → ", gflops, " GFLOPS"
    
    if (gflops > run2_gflops * 1.05) then
      print *, "  ✅ STATIC_DRAW improves performance!"
    else
      print *, "  → No difference from DYNAMIC_COPY"
    end if
  end block
  
  print *, ""
  print *, "================================================================"
  print *, "🏠 RESIDENCY DIAGNOSTIC SUMMARY"
  print *, "================================================================"
  print *, ""
  print *, "Key Findings:"
  if (speedup > 1.2) then
    print *, "  ✅ Buffers are device-local (VRAM)"
    print *, "  ✅ True zero-copy would be slower"
    print *, "  → Memory subsystem is optimized"
  else
    print *, "  ❌ Buffers may be host-visible"
    print *, "  ❌ Computing on PCIe-accessible memory"
    print *, "  → Major performance opportunity!"
  end if
  print *, ""
  print *, "Next: Run Mini's Diagnostic #3 (Register Pressure)"
  
  ! Clean up
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

end program test_residency_debug