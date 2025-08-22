program test_zero_copy_standalone
  ! Standalone Zero-Copy Conv2D Test
  ! ================================
  !
  ! Tests zero-copy implementation without interference from traditional impl
  
  use kinds
  use iso_c_binding
  use gpu_opengl_zero_copy
  implicit none
  
  ! Realistic test parameters
  integer, parameter :: N = 8        ! Batch size
  integer, parameter :: C = 64       ! Input channels  
  integer, parameter :: H = 56       ! Height
  integer, parameter :: W = 56       ! Width
  integer, parameter :: K = 128      ! Output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = H    ! Same due to padding
  integer, parameter :: W_out = W
  
  ! Data arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  
  ! Timing
  real(sp) :: time_ms, total_time
  integer :: i, warmup_runs = 3, test_runs = 10
  
  ! Performance stats
  integer(i64) :: total_flops
  real(sp) :: gflops
  
  print *, "🚀 Standalone Zero-Copy Conv2D Test"
  print *, "==================================="
  print *, ""
  
  ! Calculate workload size
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A)', "Workload: ", total_flops / 1000000, " MFLOPS"
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0)', "Conv: ", K, " filters, ", kernel_size, "x", kernel_size
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  input = input - 0.5
  weights = weights - 0.5
  
  ! Initialize zero-copy GPU
  print *, "Initializing zero-copy GPU..."
  if (.not. gpu_init_zero_copy()) then
    print *, "❌ Failed to initialize zero-copy GPU"
    stop 1
  end if
  
  print *, ""
  print *, "Running warmup..."
  
  ! Warmup
  do i = 1, warmup_runs
    time_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (time_ms < 0) then
      print *, "❌ Warmup failed!"
      stop 1
    end if
    print '(A,I0,A,F8.3,A)', "Warmup ", i, ": ", time_ms, " ms"
  end do
  
  print *, ""
  print *, "Running benchmark..."
  
  ! Benchmark
  total_time = 0.0
  do i = 1, test_runs
    time_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (time_ms < 0) then
      print *, "❌ Benchmark failed!"
      stop 1
    end if
    total_time = total_time + time_ms
    print '(A,I0,A,F8.3,A)', "Run ", i, ": ", time_ms, " ms"
  end do
  
  time_ms = total_time / test_runs
  gflops = real(total_flops) / (time_ms * 1.0e6)
  
  print *, ""
  print *, "📊 Results:"
  print *, "=========="
  print '(A,F8.3,A)', "Average time: ", time_ms, " ms"
  print '(A,F8.1,A)', "Performance:  ", gflops, " GFLOPS"
  
  ! Verify output is not all zeros
  print *, ""
  print *, "Output validation:"
  block
    real(sp) :: min_val, max_val, avg_val
    integer :: j
    
    min_val = output(1)
    max_val = output(1)
    avg_val = 0.0
    
    do j = 1, size(output)
      min_val = min(min_val, output(j))
      max_val = max(max_val, output(j))
      avg_val = avg_val + output(j)
    end do
    avg_val = avg_val / size(output)
    
    print '(A,F10.6)', "Min value: ", min_val
    print '(A,F10.6)', "Max value: ", max_val
    print '(A,F10.6)', "Avg value: ", avg_val
    
    if (abs(max_val - min_val) < 1e-6) then
      print *, "⚠️  WARNING: Output appears constant!"
    else
      print *, "✅ Output has variation"
    end if
  end block
  
  ! Cleanup
  call gpu_cleanup_zero_copy()
  deallocate(input, weights, output)
  
  print *, ""
  print *, "🏁 Test complete!"
  
end program test_zero_copy_standalone