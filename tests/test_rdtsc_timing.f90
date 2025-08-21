program test_rdtsc_timing
  ! Test using CPU cycle counters (RDTSC) for ultra-precise timing
  ! This bypasses OS overhead and gives us raw cycle counts
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_opengl
  use flopcount
  implicit none
  
  ! Interface to inline assembly for RDTSC
  interface
    function rdtsc() bind(C, name='rdtsc_wrapper')
      import :: c_int64_t
      integer(c_int64_t) :: rdtsc
    end function
  end interface
  
  integer, parameter :: batch = 1, in_c = 256, out_c = 512
  integer, parameter :: h = 256, w = 256, kh = 3, kw = 3
  
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: h_out, w_out, i
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles_start, cycles_end, cycles_elapsed
  real(dp) :: cpu_ghz, time_ns, gflops
  logical :: success
  
  print *, "=== RDTSC Cycle-Accurate GPU Timing ==="
  print *, ""
  print *, "Using CPU timestamp counter to bypass OS overhead"
  print *, "This gives us nanosecond precision without syscalls"
  print *, ""
  
  ! Estimate CPU frequency (assume ~3.5 GHz for modern CPUs)
  cpu_ghz = 3.5_dp
  print '(A,F4.1,A)', "Assuming CPU frequency: ", cpu_ghz, " GHz"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  ! Setup arrays
  h_out = h - kh + 1
  w_out = w - kw + 1
  
  allocate(input(batch * in_c * h * w))
  allocate(kernel(out_c * in_c * kh * kw))
  allocate(output(batch * out_c * h_out * w_out))
  
  call random_number(input)
  call random_number(kernel)
  
  total_flops = conv2d_flops(int(batch, i64), int(in_c, i64), int(out_c, i64), &
                            int(h_out, i64), int(w_out, i64), int(kh, i64), int(kw, i64))
  
  ! Warm up
  do i = 1, 3
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_c, out_c, h, w, kh, kw)
  end do
  
  print *, "Testing with RDTSC cycle counters..."
  print *, ""
  
  ! Measure with RDTSC
  do i = 1, 5
    cycles_start = rdtsc()
    
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_c, out_c, h, w, kh, kw)
    
    cycles_end = rdtsc()
    
    cycles_elapsed = cycles_end - cycles_start
    time_ns = real(cycles_elapsed, dp) / cpu_ghz  ! cycles / (cycles/ns) = ns
    gflops = real(total_flops, dp) / time_ns  ! FLOP / ns = GFLOPS
    
    print '(A,I0,A)', "Run ", i, ":"
    print '(A,I0,A)', "  Cycles: ", cycles_elapsed, " CPU cycles"
    print '(A,F8.3,A)', "  Time: ", time_ns / 1.0e6_dp, " ms"
    print '(A,F10.1,A)', "  Performance: ", gflops, " GFLOPS"
  end do
  
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  
end program test_rdtsc_timing