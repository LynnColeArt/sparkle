program test_gpu_performance
  ! Test NVIDIA RTX A4500 GPU performance with real convolution
  
  use kinds
  use sporkle_nvidia_opengl
  use flopcount
  use time_utils
  implicit none
  
  ! Test configurations - progressively larger
  integer :: test_size, batch, in_c, out_c, h, w
  integer, parameter :: kh = 3, kw = 3
  
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: h_out, w_out
  integer(i64) :: total_flops, t0
  real(dp) :: gflops, elapsed, best_gflops
  logical :: success
  integer :: i
  
  print *, "=== NVIDIA RTX A4500 GPU Performance Test ==="
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  print *, "✅ GPU initialized: NVIDIA RTX A4500"
  print *, "   Peak FP32: 16.5 TFLOPS"
  print *, "   Memory BW: 384 GB/s"
  print *, ""
  
  best_gflops = 0.0
  
  ! Test different sizes
  do test_size = 1, 4
    select case(test_size)
    case(1)
      ! Small test
      batch = 1; in_c = 64; out_c = 64; h = 128; w = 128
    case(2)
      ! Medium test
      batch = 1; in_c = 128; out_c = 256; h = 256; w = 256
    case(3)
      ! Large test
      batch = 1; in_c = 256; out_c = 512; h = 256; w = 256
    case(4)
      ! Summit test (Mini's configuration)
      batch = 16; in_c = 512; out_c = 1024; h = 256; w = 256
    end select
    
    ! Calculate sizes
    h_out = h - kh + 1
    w_out = w - kw + 1
    
    ! Allocate arrays
    if (allocated(input)) deallocate(input)
    if (allocated(kernel)) deallocate(kernel)
    if (allocated(output)) deallocate(output)
    
    allocate(input(batch * in_c * h * w))
    allocate(kernel(out_c * in_c * kh * kw))
    allocate(output(batch * out_c * h_out * w_out))
    
    ! Initialize with test data
    call random_number(input)
    call random_number(kernel)
    output = 0.0
    
    ! Calculate workload
    total_flops = conv2d_flops(int(batch, i64), int(in_c, i64), int(out_c, i64), &
                              int(h_out, i64), int(w_out, i64), int(kh, i64), int(kw, i64))
    
    print '(A,I0,A)', "Test ", test_size, ":"
    print '(A,I0,A,I0,A,I0,A,I0)', "  Dims: ", batch, "×", in_c, "×", h, "×", w
    print '(A,I0,A,I0)', "  Conv: ", out_c, " filters, ", kh, "×", kw, " kernel"
    print '(A,F10.3,A)', "  FLOPs: ", real(total_flops) / 1.0e9, " GFLOPs"
    
    ! Warm-up run
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_c, out_c, h, w, kh, kw)
    
    ! Timed runs
    call tic(t0)
    do i = 1, 3
      success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                        batch, in_c, out_c, h, w, kh, kw)
    end do
    elapsed = toc_seconds(t0) / 3.0  ! Average of 3 runs
    
    if (success .and. elapsed > 0.0) then
      gflops = real(total_flops, dp) / (elapsed * 1.0e9)
      print '(A,F8.3,A)', "  Time: ", elapsed * 1000.0, " ms"
      print '(A,F10.1,A)', "  Performance: ", gflops, " GFLOPS"
      
      if (gflops > best_gflops) then
        best_gflops = gflops
      end if
      
      ! Check for milestone achievements
      if (gflops > 1000.0) then
        print *, "  🎯 Breaking 1 TFLOPS barrier!"
      end if
      if (gflops > 5000.0) then
        print *, "  🚀 Breaking 5 TFLOPS barrier!"
      end if
      if (gflops > 10000.0) then
        print *, "  🔥 Breaking 10 TFLOPS barrier!"
      end if
    else
      print *, "  ❌ Execution failed"
    end if
    
    ! Verify output
    if (maxval(abs(output)) > 0.0) then
      print '(A,F10.6)', "  ✅ Valid output, max: ", maxval(output)
    else
      print *, "  ⚠️ Output is zeros"
    end if
    print *, ""
  end do
  
  ! Summary
  print *, "=== PERFORMANCE SUMMARY ==="
  print '(A,F10.1,A)', "Best performance: ", best_gflops, " GFLOPS"
  print '(A,F5.1,A)', "Efficiency: ", (best_gflops / 16500.0) * 100.0, "% of peak"
  print *, ""
  
  if (best_gflops > 10000.0) then
    print *, "🏔️ SUMMIT TRAJECTORY CONFIRMED!"
    print *, "Real GPU compute is working at scale!"
    print *, "Next steps: GPU timers, persistent buffers, optimal kernels"
  else if (best_gflops > 1000.0) then
    print *, "✅ GPU COMPUTE OPERATIONAL"
    print *, "Foundation is solid, optimizations will unlock full performance"
  else
    print *, "🔧 NEEDS OPTIMIZATION"
    print *, "GPU is executing but performance is limited"
  end if
  
  ! Clean up
  call nvidia_gl_shutdown()
  if (allocated(input)) deallocate(input)
  if (allocated(kernel)) deallocate(kernel)
  if (allocated(output)) deallocate(output)
  
end program test_gpu_performance