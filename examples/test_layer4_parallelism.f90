program test_layer4_parallelism
  use iso_fortran_env
  use cpu_conv2d_reference, only: conv2d_cpu_reference
  use sparkle_conv2d, only: conv2d_cpu
  use omp_lib
  implicit none
  
  ! Test parameters - same as before
  integer, parameter :: N = 1
  integer, parameter :: C = 64
  integer, parameter :: H = 56
  integer, parameter :: W = 56
  integer, parameter :: K = 64
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = 56
  integer, parameter :: W_out = 56
  
  ! Arrays
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size
  
  ! Timing
  real(real32) :: single_thread_time, multi_thread_time
  real(real32) :: single_thread_gflops, multi_thread_gflops
  integer(8) :: total_flops
  integer :: num_threads, i
  
  print *, "🔦 Layer 4: CPU Parallelism Test"
  print *, "================================"
  print *, ""
  
  ! Get number of available threads
  num_threads = omp_get_max_threads()
  print *, "🖥️  System Configuration:"
  print *, "   CPU: AMD Ryzen 7 7700X"
  print *, "   Physical cores: 8"
  print *, "   Logical threads:", num_threads
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  total_flops = int(N, 8) * K * H_out * W_out * C * kernel_size * kernel_size * 2
  
  ! Allocate and initialize
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  print *, "📊 Test Configuration:"
  print '(A,I3,A,I3,A,I3,A,I3)', " Input: ", N, "x", C, "x", H, "x", W
  print '(A,I3,A,I3)', " Kernel: ", kernel_size, "x", kernel_size
  print '(A,I3,A,I3,A,I3,A,I3)', " Output: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Test 1: Single-threaded (current implementation)
  print *, "🧵 Test 1: Single-threaded execution"
  call omp_set_num_threads(1)
  
  ! Warmup
  output = 0.0
  single_thread_time = conv2d_cpu_reference(input, weights, output, &
                                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Actual measurement
  output = 0.0
  single_thread_time = conv2d_cpu_reference(input, weights, output, &
                                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  single_thread_gflops = real(total_flops) / (single_thread_time * 1.0e6)
  print '(A,F8.2,A,F8.1,A)', "   Time: ", single_thread_time, " ms (", single_thread_gflops, " GFLOPS)"
  
  ! Test 2: Multi-threaded with production implementation
  print *, ""
  print *, "🚀 Test 2: Multi-threaded execution (production naive)"
  call omp_set_num_threads(num_threads)
  print *, "   Using", omp_get_max_threads(), "threads"
  
  ! Warmup
  output = 0.0
  call conv2d_cpu(input, weights, output, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Actual measurement - time it manually since conv2d_cpu doesn't return time
  call cpu_time(multi_thread_time)
  output = 0.0
  call conv2d_cpu(input, weights, output, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  call cpu_time(single_thread_time)
  multi_thread_time = (single_thread_time - multi_thread_time) * 1000.0  ! Convert to ms
  
  multi_thread_gflops = real(total_flops) / (multi_thread_time * 1.0e6)
  print '(A,F8.2,A,F8.1,A)', "   Time: ", multi_thread_time, " ms (", multi_thread_gflops, " GFLOPS)"
  
  ! Summary
  print *, ""
  print *, "📈 Performance Summary:"
  print '(A,F8.1,A)', "  Single-threaded: ", single_thread_gflops, " GFLOPS"
  print '(A,F8.1,A)', "  Multi-threaded:  ", multi_thread_gflops, " GFLOPS"
  print '(A,F6.2,A)', "  Speedup: ", multi_thread_gflops / single_thread_gflops, "x"
  print *, ""
  
  if (multi_thread_gflops > 100.0) then
    print *, "✅ Parallelism working! >100 GFLOPS achieved on CPU!"
    print *, "   This is what we've been missing!"
  else if (multi_thread_gflops > 50.0) then
    print *, "🔶 Good parallel performance, but room for improvement"
  else
    print *, "❌ Parallel performance lower than expected"
  end if
  
  print *, ""
  print *, "💡 Key Insight:"
  print *, "   Each CPU thread is a 'compute device' in our unified model"
  print *, "   The same scheduler that distributes to threads will distribute to GPUs"
  
  ! Test different thread counts
  print *, ""
  print *, "🔬 Thread Scaling Test:"
  do i = 1, 5
    select case(i)
    case(1); call omp_set_num_threads(1)
    case(2); call omp_set_num_threads(2)
    case(3); call omp_set_num_threads(4)
    case(4); call omp_set_num_threads(8)
    case(5); call omp_set_num_threads(16)
    end select
    
    call cpu_time(multi_thread_time)
    output = 0.0
    call conv2d_cpu(input, weights, output, &
                    N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    call cpu_time(single_thread_time)
    multi_thread_time = (single_thread_time - multi_thread_time) * 1000.0
    multi_thread_gflops = real(total_flops) / (multi_thread_time * 1.0e6)
    
    print '(A,I3,A,F8.1,A)', "   Threads: ", omp_get_max_threads(), " → ", multi_thread_gflops, " GFLOPS"
  end do
  
  ! Cleanup
  deallocate(input, weights, output)
  
end program test_layer4_parallelism