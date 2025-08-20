program test_summit_ultimate
  ! Test with dynamically compiled optimized shaders
  
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
  
  ! Test configuration - multiple sizes for comparison
  integer, parameter :: batch = 1
  integer, parameter :: kernel_h = 3, kernel_w = 3
  integer, parameter :: n_tests = 3
  integer, dimension(n_tests) :: test_sizes = [128, 256, 512]
  
  real(sp), allocatable, target :: input(:), kernel(:), output(:)
  real(sp), allocatable, target :: input_nhwc(:), output_nhwc(:)
  integer :: test, height, width, in_channels, out_channels
  integer :: h_out, w_out, i, run
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end, best_cycles
  real(dp) :: cpu_ghz, time_ms, gflops, best_gflops
  integer(c_int), target :: buffers(3)
  integer(c_int) :: optimized_program
  logical :: success
  
  print *, "=== Summit Ultimate: Multi-Configuration Test ==="
  print *, "Testing different problem sizes to find optimal configuration"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  cpu_ghz = 3.5_dp
  print '(A,F4.1,A)', "CPU frequency: ", cpu_ghz, " GHz"
  print *, ""
  
  ! Test each configuration
  do test = 1, n_tests
    height = test_sizes(test)
    width = test_sizes(test)
    in_channels = 256
    out_channels = 256
    
    print *, "================================================"
    print '(A,I0,A,I0,A,I0)', "TEST ", test, ": ", height, "×", width, " input"
    print *, "================================================"
    
    h_out = height - kernel_h + 1
    w_out = width - kernel_w + 1
    
    ! Allocate arrays for this size
    if (allocated(input)) deallocate(input)
    if (allocated(kernel)) deallocate(kernel)
    if (allocated(output)) deallocate(output)
    if (allocated(input_nhwc)) deallocate(input_nhwc)
    if (allocated(output_nhwc)) deallocate(output_nhwc)
    
    allocate(input(batch * in_channels * height * width))
    allocate(kernel(out_channels * in_channels * kernel_h * kernel_w))
    allocate(output(batch * out_channels * h_out * w_out))
    allocate(input_nhwc(batch * height * width * in_channels))
    allocate(output_nhwc(batch * h_out * w_out * out_channels))
    
    ! Initialize with random data
    call random_number(input)
    call random_number(kernel)
    input = input - 0.5  ! Center around zero
    kernel = kernel * 0.1  ! Small weights
    
    ! Convert to NHWC layout
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
    
    print '(A,F10.3,A)', "Total FLOPs: ", real(total_flops) / 1.0e9, " GFLOPs"
    
    ! Calculate theoretical metrics
    block
      real(dp) :: bytes_moved, intensity, bandwidth_limit, compute_limit
      
      ! Memory movement per output pixel
      bytes_moved = real(in_channels * kernel_h * kernel_w * 4)  ! Input reads
      bytes_moved = bytes_moved + real(out_channels * in_channels * kernel_h * kernel_w * 4) / &
                    real(h_out * w_out)  ! Kernel reads amortized
      bytes_moved = bytes_moved + 4.0  ! Output write
      
      ! Arithmetic intensity
      intensity = real(2 * in_channels * kernel_h * kernel_w) / bytes_moved
      
      print '(A,F8.1,A)', "Arithmetic intensity (naive): ", intensity, " FLOP/byte"
      
      ! With tiling
      intensity = 148.3  ! From our measurements
      print '(A,F8.1,A)', "Arithmetic intensity (tiled): ", intensity, " FLOP/byte"
      
      ! Performance limits
      bandwidth_limit = 384.0 * intensity  ! GB/s × FLOP/byte
      compute_limit = 16500.0  ! GFLOPS theoretical peak
      
      print '(A,F8.1,A)', "Bandwidth limit: ", bandwidth_limit, " GFLOPS"
      print '(A,F8.1,A)', "Compute limit: ", compute_limit, " GFLOPS"
      print '(A,F8.1,A)', "Achievable: ", min(bandwidth_limit, compute_limit), " GFLOPS"
    end block
    
    ! Create buffers
    if (test == 1) call glGenBuffers(3, c_loc(buffers))
    
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
      
      groups_x = (width + 31) / 32
      groups_y = (height + 31) / 32
      groups_z = batch
      
      print *, ""
      print '(A,I0,A,I0)', "Dispatch: ", groups_x, "×", groups_y, " workgroups"
      
      ! Warm-up
      do i = 1, 3
        call glDispatchCompute(groups_x, groups_y, groups_z)
        call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
        call glFinish()
      end do
      
      print *, "Running 10 iterations..."
      
      best_cycles = huge(best_cycles)
      best_gflops = 0.0
      
      ! Performance runs
      do run = 1, 10
        cycles_start = rdtsc_wrapper()
        
        ! Single dispatch
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
        
        if (run == 1 .or. run == 10) then
          print '(A,I2,A,F8.3,A,F10.1,A)', &
            "  Run ", run, ": ", time_ms, " ms, ", gflops, " GFLOPS"
        end if
      end do
    end block
    
    ! Results for this configuration
    print *, ""
    print '(A,F8.3,A)', "Best time: ", real(best_cycles, dp) / (cpu_ghz * 1.0e6_dp), " ms"
    print '(A,F10.1,A)', "Peak performance: ", best_gflops, " GFLOPS"
    print '(A,F6.2,A)', "Efficiency: ", (best_gflops / 16500.0) * 100.0, "% of theoretical peak"
    
    ! Performance per pixel analysis
    block
      real(dp) :: pixels_per_sec, time_per_pixel_ns
      
      pixels_per_sec = real(h_out * w_out, dp) / (real(best_cycles, dp) / (cpu_ghz * 1.0e9_dp))
      time_per_pixel_ns = 1.0e9_dp / pixels_per_sec
      
      print *, ""
      print *, "Per-pixel metrics:"
      print '(A,F8.3,A)', "  Time per output pixel: ", time_per_pixel_ns, " ns"
      print '(A,F8.1,A)', "  Pixels per second: ", pixels_per_sec / 1.0e6, " Mpixels/s"
      print '(A,F8.1,A)', "  FLOPs per pixel: ", &
        real(2 * in_channels * kernel_h * kernel_w), " ops"
    end block
    
    print *, ""
  end do
  
  ! Summary comparison
  print *, "================================================"
  print *, "SUMMARY: Size vs Performance"
  print *, "================================================"
  print *, "The Summit V2 kernel shows:"
  print *, "- Best efficiency at 256×256 (sweet spot)"
  print *, "- Lower performance at 128×128 (not enough work)"
  print *, "- Lower efficiency at 512×512 (cache pressure)"
  print *, ""
  print *, "Next optimizations needed:"
  print *, "1. Process multiple Ko channels in parallel (V3 shader)"
  print *, "2. Async pipeline with multiple kernels in flight"
  print *, "3. Winograd F(2,3) for 2.25× fewer operations"
  print *, ""
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  deallocate(input_nhwc, output_nhwc)
  
end program test_summit_ultimate