program test_gpu_compute
  ! Test actual GPU compute execution
  
  use kinds
  use sporkle_nvidia_opengl
  use flopcount
  use time_utils
  implicit none
  
  ! Test configuration - small for quick testing
  integer, parameter :: batch = 1, in_c = 64, out_c = 64
  integer, parameter :: h = 32, w = 32, kh = 3, kw = 3
  
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: h_out, w_out
  integer(i64) :: total_flops, t0
  real(dp) :: gflops, elapsed
  logical :: success
  
  print *, "=== NVIDIA RTX A4500 GPU Compute Test ==="
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  print *, "✅ GPU initialized successfully"
  print *, ""
  
  ! Calculate sizes
  h_out = h - kh + 1
  w_out = w - kw + 1
  
  ! Allocate arrays
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
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Workload: ", batch, "×", in_c, "×", h, "×", w
  print '(A,I0,A,I0)', "Kernel: ", kh, "×", kw
  print '(A,F8.3,A)', "Total FLOPs: ", real(total_flops) / 1.0e6, " MFLOPs"
  print *, ""
  
  ! Execute on GPU
  print *, "Executing convolution on GPU..."
  call tic(t0)
  
  success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                    batch, in_c, out_c, h, w, kh, kw)
  
  elapsed = toc_seconds(t0)
  
  if (success) then
    print *, "✅ GPU execution successful!"
    print '(A,F8.3,A)', "Total time: ", elapsed * 1000.0, " ms"
    
    if (elapsed > 0.0) then
      gflops = real(total_flops, dp) / (elapsed * 1.0e9)
      print '(A,F10.3,A)', "Performance: ", gflops, " GFLOPS"
      
      if (gflops > 100.0) then
        print *, ""
        print *, "🚀 REAL GPU COMPUTE WORKING!"
        print *, "We're executing actual compute shaders on the RTX A4500!"
      end if
    end if
  else
    print *, "❌ GPU execution failed"
  end if
  
  ! Verify output has data (simple sanity check)
  if (maxval(abs(output)) > 0.0) then
    print *, ""
    print *, "✅ Output contains computed data"
    print '(A,F10.6)', "Max output value: ", maxval(output)
  else
    print *, "⚠️ Output is all zeros - shader may not be executing"
  end if
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  
  print *, ""
  print *, "Test complete!"
  
end program test_gpu_compute