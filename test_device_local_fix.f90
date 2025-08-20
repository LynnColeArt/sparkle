program test_device_local_fix
  ! Fix the host-visible memory trap - force true VRAM allocation
  ! Based on Mini's guidance: device-local SSBOs + staging ring
  
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
    
    ! Buffer copy interface
    subroutine glCopyBufferSubData(read_target, write_target, read_offset, write_offset, size) bind(C, name='glCopyBufferSubData')
      import :: c_int, c_size_t
      integer(c_int), value :: read_target
      integer(c_int), value :: write_target
      integer(c_size_t), value :: read_offset
      integer(c_size_t), value :: write_offset
      integer(c_size_t), value :: size
    end subroutine
  end interface
  
  ! OpenGL constants for memory management
  integer(c_int), parameter :: GL_STATIC_DRAW = int(z'88E4', c_int)
  integer(c_int), parameter :: GL_STREAM_DRAW = int(z'88E0', c_int)
  integer(c_int), parameter :: GL_READ_WRITE = int(z'88BA', c_int)
  integer(c_int), parameter :: GL_WRITE_ONLY = int(z'88B9', c_int)
  integer(c_int), parameter :: GL_COPY_READ_BUFFER = int(z'8F36', c_int)
  integer(c_int), parameter :: GL_COPY_WRITE_BUFFER = int(z'8F37', c_int)
  integer(c_int), parameter :: GL_BUFFER_UPDATE_BARRIER_BIT = int(z'00000200', c_int)
  
  ! Test configuration
  integer, parameter :: batch = 1
  integer, parameter :: height = 256, width = 256
  integer, parameter :: in_channels = 256, out_channels = 256
  integer, parameter :: kernel_h = 3, kernel_w = 3
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: h_out, w_out, i
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end
  real(dp) :: cpu_ghz, time_ms, gflops
  real(dp) :: baseline_gflops, device_local_gflops, speedup
  
  ! Buffer management
  integer(c_int), target :: compute_buffers(3)    ! Device-local compute buffers
  integer(c_int), target :: staging_buffers(3)    ! Host-visible staging buffers
  type(c_ptr) :: mapped_ptr
  
  print *, "================================================================"
  print *, "🔧 FIXING THE MEMORY RESIDENCY TRAP"
  print *, "================================================================"
  print *, ""
  print *, "Mini's Fix: True device-local VRAM allocation"
  print *, "  🎯 Device-local SSBOs for compute (not host-visible)"
  print *, "  🎯 Persistent staging ring with explicit flush"
  print *, "  🎯 Explicit copy to device instead of coherent mapping"
  print *, "  🎯 Target: >1.2× speedup when data is resident"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  cpu_ghz = 3.5_dp
  
  ! Setup data
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
  
  call glUseProgram(conv2d_program)
  call set_uniforms()
  
  ! === BASELINE: Current approach ===
  print *, "📊 BASELINE: Current GL_DYNAMIC_COPY approach"
  print *, "============================================="
  
  call test_current_approach(baseline_gflops)
  
  print '(A,F10.1,A)', "Baseline performance: ", baseline_gflops, " GFLOPS"
  print *, ""
  
  ! === OPTIMIZED: Device-local + staging ring ===
  print *, "🚀 OPTIMIZED: Device-local + staging ring"
  print *, "=========================================="
  
  call test_device_local_approach(device_local_gflops)
  
  print '(A,F10.1,A)', "Device-local performance: ", device_local_gflops, " GFLOPS"
  print *, ""
  
  ! === ANALYSIS ===
  print *, "================================================================"
  print *, "🔧 MEMORY RESIDENCY FIX RESULTS"
  print *, "================================================================"
  print *, ""
  
  speedup = device_local_gflops / baseline_gflops
  
  print '(A,F10.1,A)', "Baseline (GL_DYNAMIC_COPY): ", baseline_gflops, " GFLOPS"
  print '(A,F10.1,A)', "Device-local (staged):      ", device_local_gflops, " GFLOPS"
  print '(A,F5.2,A)', "Speedup: ", speedup, "×"
  print *, ""
  
  if (speedup > 1.5) then
    print *, "🎉 MAJOR BREAKTHROUGH!"
    print *, "   Device-local allocation delivers massive speedup"
    print *, "   Memory residency trap successfully fixed!"
    print *, "   This is the path to 17-19 TFLOPS!"
  else if (speedup > 1.2) then
    print *, "✅ SIGNIFICANT IMPROVEMENT!"
    print *, "   Device-local allocation working"
    print *, "   Confirms Mini's residency fix strategy"
  else if (speedup > 1.05) then
    print *, "⚠️  MODEST IMPROVEMENT"
    print *, "   Some benefit but not fully optimized"
    print *, "   May need additional memory hints"
  else
    print *, "❌ NO IMPROVEMENT"
    print *, "   Still hitting host-visible memory"
    print *, "   Need stronger device-local allocation strategy"
  end if
  
  print *, ""
  print *, "Next steps:"
  print *, "  🔸 Test with larger workloads (512×512)"
  print *, "  🔸 Implement non-coherent staging ring"
  print *, "  🔸 Fix GPU query ring buffer"
  print *, "  🔸 Reduce dispatch overhead"
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  deallocate(input_nhwc, output_nhwc)

contains

  subroutine test_current_approach(result_gflops)
    real(dp), intent(out) :: result_gflops
    integer(c_int), target :: buffers(3)
    integer :: groups_x, groups_y, groups_z
    
    ! Standard GL_DYNAMIC_COPY approach (what we've been using)
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
    
    groups_x = (width + 31) / 32
    groups_y = (height + 31) / 32
    groups_z = batch
    
    ! Warm up
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    ! Measure (compute only, no upload)
    cycles_start = rdtsc_wrapper()
    
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    cycles_end = rdtsc_wrapper()
    
    time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
    result_gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
    
    ! Buffers cleaned up automatically on context destruction
  end subroutine test_current_approach
  
  subroutine test_device_local_approach(result_gflops)
    real(dp), intent(out) :: result_gflops
    integer :: groups_x, groups_y, groups_z
    
    ! Create device-local compute buffers (force VRAM allocation)
    call glGenBuffers(3, c_loc(compute_buffers))
    call glGenBuffers(3, c_loc(staging_buffers))
    
    print *, "  Creating device-local compute buffers..."
    
    ! Input buffer: device-local for compute
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, compute_buffers(1))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(batch * height * width * in_channels * 4, c_size_t), &
                     c_null_ptr, GL_STATIC_DRAW)  ! NULL data = force device allocation
    
    ! Kernel buffer: device-local for compute  
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, compute_buffers(2))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(out_channels * in_channels * 9 * 4, c_size_t), &
                     c_null_ptr, GL_STATIC_DRAW)
    
    ! Output buffer: device-local for compute
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, compute_buffers(3))
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(batch * h_out * w_out * out_channels * 4, c_size_t), &
                     c_null_ptr, GL_STATIC_DRAW)
    
    print *, "  Creating staging buffers for upload..."
    
    ! Staging buffers: host-visible for upload only
    call glBindBuffer(GL_COPY_READ_BUFFER, staging_buffers(1))
    call glBufferData(GL_COPY_READ_BUFFER, &
                     int(batch * height * width * in_channels * 4, c_size_t), &
                     c_loc(input_nhwc), GL_STREAM_DRAW)  ! Host data, stream usage
    
    call glBindBuffer(GL_COPY_READ_BUFFER, staging_buffers(2))
    call glBufferData(GL_COPY_READ_BUFFER, &
                     int(out_channels * in_channels * 9 * 4, c_size_t), &
                     c_loc(kernel), GL_STREAM_DRAW)
    
    print *, "  Explicit copy: staging → device-local..."
    
    ! Explicit copy from staging to device-local (not coherent mapping!)
    call glBindBuffer(GL_COPY_READ_BUFFER, staging_buffers(1))
    call glBindBuffer(GL_COPY_WRITE_BUFFER, compute_buffers(1))
    call glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_COPY_WRITE_BUFFER, &
                            0_c_size_t, 0_c_size_t, &
                            int(batch * height * width * in_channels * 4, c_size_t))
    
    call glBindBuffer(GL_COPY_READ_BUFFER, staging_buffers(2))
    call glBindBuffer(GL_COPY_WRITE_BUFFER, compute_buffers(2))
    call glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_COPY_WRITE_BUFFER, &
                            0_c_size_t, 0_c_size_t, &
                            int(out_channels * in_channels * 9 * 4, c_size_t))
    
    ! Bind device-local buffers for compute
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, compute_buffers(1))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, compute_buffers(2))
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, compute_buffers(3))
    
    ! Ensure copy is complete
    call glMemoryBarrier(GL_BUFFER_UPDATE_BARRIER_BIT)
    
    groups_x = (width + 31) / 32
    groups_y = (height + 31) / 32
    groups_z = batch
    
    print *, "  Warming up device-local compute..."
    
    ! Warm up
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    print *, "  Measuring device-local performance..."
    
    ! Measure device-local compute performance
    cycles_start = rdtsc_wrapper()
    
    call glDispatchCompute(groups_x, groups_y, groups_z)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    cycles_end = rdtsc_wrapper()
    
    time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
    result_gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
    
    ! Buffers cleaned up automatically on context destruction
  end subroutine test_device_local_approach
  
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

end program test_device_local_fix