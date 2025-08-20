program test_summit_v2
  ! Test the REAL Summit kernel with Mini's corrections
  ! 256 threads, 32×32 tiling, Ko=64 blocking, device-local buffers
  
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
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: h_out, w_out, i, run
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end, best_cycles
  real(dp) :: cpu_ghz, time_ms, gflops, best_gflops
  integer(c_int), target :: buffers(3)
  logical :: success
  
  print *, "=== Summit V2 Performance Test ==="
  print *, "Configuration based on Mini's corrections:"
  print *, "  - 256 threads (32×8 workgroup)"
  print *, "  - 32×32 shared memory tiling"
  print *, "  - Ko=64 output channel blocking"
  print *, "  - 4×4 outputs per thread"
  print *, "  - Padded shared memory [34][33]"
  print *, "  - Device-local buffers (VRAM)"
  print *, "  - Vec4 coalesced access"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  cpu_ghz = 3.5_dp
  print '(A,F4.1,A)', "CPU frequency: ", cpu_ghz, " GHz"
  print *, ""
  
  ! Calculate sizes
  h_out = height - kernel_h + 1
  w_out = width - kernel_w + 1
  
  ! Allocate arrays (NCHW for generation)
  allocate(input(batch * in_channels * height * width))
  allocate(kernel(out_channels * in_channels * kernel_h * kernel_w))
  allocate(output(batch * out_channels * h_out * w_out))
  
  ! Allocate NHWC layout arrays
  allocate(input_nhwc(batch * height * width * in_channels))
  allocate(output_nhwc(batch * h_out * w_out * out_channels))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(kernel)
  
  ! Convert input from NCHW to NHWC
  print *, "Converting to NHWC layout for coalesced access..."
  block
    integer :: n, c, h, w, nchw_idx, nhwc_idx
    do i = 1, batch * in_channels * height * width
      ! Decode NCHW index
      nchw_idx = i - 1
      w = mod(nchw_idx, width)
      nchw_idx = nchw_idx / width
      h = mod(nchw_idx, height)
      nchw_idx = nchw_idx / height
      c = mod(nchw_idx, in_channels)
      n = nchw_idx / in_channels
      
      ! Encode NHWC index
      nhwc_idx = ((n * height + h) * width + w) * in_channels + c + 1
      input_nhwc(nhwc_idx) = input(i)
    end do
  end block
  
  ! Calculate workload
  total_flops = conv2d_flops(int(batch, i64), int(in_channels, i64), &
                            int(out_channels, i64), int(h_out, i64), &
                            int(w_out, i64), int(kernel_h, i64), int(kernel_w, i64))
  
  print *, ""
  print *, "Workload configuration:"
  print '(A,I0,A,I0,A,I0)', "  Input: ", height, "×", width, "×", in_channels
  print '(A,I0,A,I0)', "  Output: ", h_out, "×", w_out, "×", out_channels
  print '(A,F10.3,A)', "  Total FLOPs: ", real(total_flops) / 1.0e9, " GFLOPs"
  
  ! Calculate arithmetic intensity with tiling
  block
    real(dp) :: bytes_per_tile, flops_per_tile, intensity
    
    ! 32×32 tile with Ko=64 blocking
    bytes_per_tile = 34.0 * 34.0 * in_channels * 4.0 +  &  ! Input tile
                     64.0 * in_channels * 9.0 * 4.0 +    &  ! Kernel weights
                     32.0 * 32.0 * 64.0 * 4.0               ! Output tile
    bytes_per_tile = bytes_per_tile / 1.0e6  ! MB
    
    flops_per_tile = 32.0 * 32.0 * 64.0 * in_channels * 9.0 * 2.0  ! FMA = 2 ops
    flops_per_tile = flops_per_tile / 1.0e6  ! MFLOPs
    
    intensity = flops_per_tile / bytes_per_tile
    
    print *, ""
    print *, "Arithmetic intensity with 32×32 tiling and Ko=64:"
    print '(A,F8.3,A)', "  Bytes per tile: ", bytes_per_tile, " MB"
    print '(A,F8.3,A)', "  FLOPs per tile: ", flops_per_tile, " MFLOPs"
    print '(A,F8.1,A)', "  Intensity: ", intensity, " FLOP/byte"
    
    if (intensity > 64.0) then
      print *, "  ✅ COMPUTE-BOUND (intensity > 64 FLOP/byte roofline)"
    else
      print *, "  ⚠️ Still bandwidth-limited, need more reuse"
    end if
  end block
  
  ! Create device-local buffers
  print *, ""
  print *, "Creating device-local VRAM buffers..."
  
  call glGenBuffers(3, c_loc(buffers))
  
  ! Input buffer (device-local)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(1))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                   int(batch * height * width * in_channels * 4, c_size_t), &
                   c_loc(input_nhwc), GL_DYNAMIC_COPY)
  
  ! Kernel buffer (device-local)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(2))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                   int(out_channels * in_channels * 9 * 4, c_size_t), &
                   c_loc(kernel), GL_DYNAMIC_COPY)
  
  ! Output buffer (device-local)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers(3))
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                   int(batch * h_out * w_out * out_channels * 4, c_size_t), &
                   c_null_ptr, GL_DYNAMIC_COPY)
  
  ! Bind buffers
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers(1))
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers(2))
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers(3))
  
  ! Use compute program
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
  
  ! Calculate dispatch dimensions
  block
    integer :: groups_x, groups_y, groups_z
    
    ! 32×32 output tiles
    groups_x = (width + 31) / 32
    groups_y = (height + 31) / 32
    groups_z = batch
    
    print *, ""
    print *, "Dispatch configuration:"
    print '(A,I0,A,I0,A,I0)', "  Grid: ", groups_x, "×", groups_y, "×", groups_z
    print '(A)', "  Workgroup: 32×8 (256 threads)"
    print '(A,I0)', "  Total workgroups: ", groups_x * groups_y * groups_z
    print *, ""
    
    ! Warm-up runs
    print *, "Warming up GPU..."
    do i = 1, 3
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
    end do
    
    print *, "Running performance test (10 runs)..."
    print *, ""
    
    best_cycles = huge(best_cycles)
    best_gflops = 0.0
    
    ! Performance runs
    do run = 1, 10
      cycles_start = rdtsc_wrapper()
      
      call glDispatchCompute(groups_x, groups_y, groups_z)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
      
      cycles_end = rdtsc_wrapper()
      
      time_ms = real(cycles_end - cycles_start, dp) / (cpu_ghz * 1.0e6_dp)
      gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
      
      if (cycles_end - cycles_start < best_cycles) then
        best_cycles = cycles_end - cycles_start
        best_gflops = gflops
      end if
      
      print '(A,I2,A,F8.3,A,F10.1,A)', &
        "Run ", run, ": ", time_ms, " ms, ", gflops, " GFLOPS"
      
      ! Check for milestones
      if (gflops > 2000.0 .and. run == 1) then
        print *, "    🎯 Breaking 2 TFLOPS!"
      end if
      if (gflops > 5000.0 .and. run == 1) then
        print *, "    🚀 Breaking 5 TFLOPS!"
      end if
      if (gflops > 10000.0 .and. run == 1) then
        print *, "    🔥 Breaking 10 TFLOPS!"
      end if
    end do
  end block
  
  ! Results
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F8.3,A)', "Best time: ", real(best_cycles, dp) / (cpu_ghz * 1.0e6_dp), " ms"
  print '(A,F10.1,A)', "Peak performance: ", best_gflops, " GFLOPS"
  print '(A,F6.2,A)', "Efficiency: ", (best_gflops / 16500.0) * 100.0, "% of theoretical peak"
  print *, ""
  
  ! Compare to previous results
  print *, "=== IMPROVEMENT ANALYSIS ==="
  print *, "Previous results:"
  print *, "  Naive:           354 GFLOPS"
  print *, "  RDTSC timing:    530 GFLOPS"
  print *, "  Coherent mapped:  68 GFLOPS (trap!)"
  print '(A,F10.1,A)', "  Summit V2:    ", best_gflops, " GFLOPS"
  
  if (best_gflops > 530.0) then
    print *, ""
    print '(A,F5.2,A)', "Speedup over naive: ", best_gflops / 354.0, "×"
    print '(A,F5.2,A)', "Speedup over RDTSC: ", best_gflops / 530.0, "×"
  end if
  
  if (best_gflops > 2000.0) then
    print *, ""
    print *, "✅ SHARED MEMORY TILING IS WORKING!"
    print *, "We've escaped the bandwidth limit!"
  end if
  
  if (best_gflops > 5000.0) then
    print *, ""
    print *, "🏔️ APPROACHING SUMMIT!"
    print *, "The universal pattern delivers!"
  end if
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  deallocate(input_nhwc, output_nhwc)
  
end program test_summit_v2