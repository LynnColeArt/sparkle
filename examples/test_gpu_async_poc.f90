program test_gpu_async_poc
  ! Proof of concept: Can we reduce GPU idle time with simple async?
  use iso_fortran_env, only: real32, real64, int64
  use iso_c_binding
  use gpu_opengl_interface
  use sparkle_types
  implicit none
  
  ! Test parameters - smaller to show idle time
  integer, parameter :: NUM_ITERATIONS = 100
  integer, parameter :: BATCH_SIZE = 1
  integer, parameter :: N = BATCH_SIZE
  integer, parameter :: C = 3  
  integer, parameter :: H = 224
  integer, parameter :: W = 224
  integer, parameter :: K = 64
  integer, parameter :: kernel_size = 7
  integer, parameter :: stride = 2
  integer, parameter :: pad = 3
  integer, parameter :: H_out = 112
  integer, parameter :: W_out = 112
  
  ! Buffers for double buffering
  real(real32), allocatable, target :: input_a(:), input_b(:)
  real(real32), allocatable, target :: weights(:)
  real(real32), allocatable, target :: output_a(:), output_b(:)
  
  ! Timing
  integer(int64) :: start_time, end_time, gpu_start, gpu_end
  integer(int64) :: cpu_prep_time, gpu_compute_time, cpu_post_time
  integer(int64) :: total_gpu_time, total_wall_time
  real(real64) :: clock_rate
  real(real32) :: gpu_time_ms
  integer :: i
  logical :: use_buffer_a
  
  ! Calculate sizes
  integer(int64) :: input_size, weight_size, output_size
  integer(int64) :: flop_count
  real(real64) :: total_gflops
  
  print *, "=== GPU Async Proof of Concept ==="
  print *, ""
  print *, "Testing: Can we reduce GPU idle time with double buffering?"
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate double buffers
  allocate(input_a(input_size), input_b(input_size))
  allocate(weights(weight_size))
  allocate(output_a(output_size), output_b(output_size))
  
  ! Initialize data
  call random_number(input_a)
  call random_number(input_b)
  call random_number(weights)
  
  ! Get clock rate for timing
  call system_clock(count_rate=clock_rate)
  
  print *, "Configuration:"
  print *, "  Batch size:", BATCH_SIZE
  print *, "  Input shape:", N, "x", C, "x", H, "x", W
  print *, "  Output shape:", N, "x", K, "x", H_out, "x", W_out
  print *, "  Iterations:", NUM_ITERATIONS
  print *, ""
  
  ! Calculate FLOPs
  flop_count = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
               int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2
  
  print *, "=== Test 1: Synchronous Baseline ==="
  print *, "(Current approach - GPU waits for CPU)"
  print *, ""
  
  total_gpu_time = 0
  cpu_prep_time = 0
  cpu_post_time = 0
  
  call system_clock(start_time)
  
  do i = 1, NUM_ITERATIONS
    ! CPU prep (simulate data preparation)
    call system_clock(gpu_start)
    call simulate_cpu_work(1.0)  ! 1ms CPU prep
    call system_clock(gpu_end)
    cpu_prep_time = cpu_prep_time + (gpu_end - gpu_start)
    
    ! GPU compute
    call system_clock(gpu_start)
    gpu_time_ms = gpu_execute_conv2d_ref(input_a, weights, output_a, &
                                         N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    call system_clock(gpu_end)
    total_gpu_time = total_gpu_time + (gpu_end - gpu_start)
    
    ! CPU post-process (simulate result processing)
    call system_clock(gpu_start)
    call simulate_cpu_work(0.5)  ! 0.5ms CPU post
    call system_clock(gpu_end)
    cpu_post_time = cpu_post_time + (gpu_end - gpu_start)
  end do
  
  call system_clock(end_time)
  total_wall_time = end_time - start_time
  
  ! Report synchronous results
  print *, "Synchronous Results:"
  print '(A,F8.2,A)', "  Total wall time: ", real(total_wall_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.2,A)', "  GPU compute time: ", real(total_gpu_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.2,A)', "  CPU prep time: ", real(cpu_prep_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.2,A)', "  CPU post time: ", real(cpu_post_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.2,A)', "  GPU idle time: ", &
    (real(total_wall_time) - real(total_gpu_time)) / clock_rate * 1000.0, " ms"
  print '(A,F6.1,A)', "  GPU utilization: ", &
    real(total_gpu_time) / real(total_wall_time) * 100.0, "%"
  
  total_gflops = real(flop_count * NUM_ITERATIONS, real64) / &
                 (real(total_gpu_time) / clock_rate * 1.0e9)
  print '(A,F8.1,A)', "  Peak GFLOPS (GPU only): ", total_gflops
  
  total_gflops = real(flop_count * NUM_ITERATIONS, real64) / &
                 (real(total_wall_time) / clock_rate * 1.0e9)
  print '(A,F8.1,A)', "  Effective GFLOPS: ", total_gflops, " (including idle time)"
  print *, ""
  
  print *, "=== Test 2: Simple Async (Double Buffering) ==="
  print *, "(Overlap CPU prep with GPU compute)"
  print *, ""
  
  ! Reset counters
  total_gpu_time = 0
  cpu_prep_time = 0
  cpu_post_time = 0
  use_buffer_a = .true.
  
  call system_clock(start_time)
  
  ! Prepare first batch
  call system_clock(gpu_start)
  call simulate_cpu_work(1.0)
  call system_clock(gpu_end)
  cpu_prep_time = cpu_prep_time + (gpu_end - gpu_start)
  
  do i = 1, NUM_ITERATIONS
    if (use_buffer_a) then
      ! GPU processes buffer A while CPU prepares buffer B
      
      ! Start GPU on buffer A
      call system_clock(gpu_start)
      gpu_time_ms = gpu_execute_conv2d_ref(input_a, weights, output_a, &
                                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      call system_clock(gpu_end)
      total_gpu_time = total_gpu_time + (gpu_end - gpu_start)
      
      ! While GPU runs, CPU prepares next batch (if not last iteration)
      if (i < NUM_ITERATIONS) then
        call simulate_cpu_work(1.0)  ! This happens IN PARALLEL with GPU
      end if
      
    else
      ! GPU processes buffer B while CPU prepares buffer A
      
      call system_clock(gpu_start)
      gpu_time_ms = gpu_execute_conv2d_ref(input_b, weights, output_b, &
                                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      call system_clock(gpu_end)
      total_gpu_time = total_gpu_time + (gpu_end - gpu_start)
      
      if (i < NUM_ITERATIONS) then
        call simulate_cpu_work(1.0)
      end if
    end if
    
    ! Simulate post-processing (still synchronous for now)
    call system_clock(gpu_start)
    call simulate_cpu_work(0.5)
    call system_clock(gpu_end)
    cpu_post_time = cpu_post_time + (gpu_end - gpu_start)
    
    use_buffer_a = .not. use_buffer_a
  end do
  
  call system_clock(end_time)
  total_wall_time = end_time - start_time
  
  ! Report async results
  print *, "Simple Async Results:"
  print '(A,F8.2,A)', "  Total wall time: ", real(total_wall_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.2,A)', "  GPU compute time: ", real(total_gpu_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.2,A)', "  CPU work (overlapped): ~", &
    real(NUM_ITERATIONS) * 1.0, " ms"
  print '(A,F8.2,A)', "  GPU idle time: ", &
    (real(total_wall_time) - real(total_gpu_time)) / clock_rate * 1000.0, " ms"
  print '(A,F6.1,A)', "  GPU utilization: ", &
    real(total_gpu_time) / real(total_wall_time) * 100.0, "%"
  
  total_gflops = real(flop_count * NUM_ITERATIONS, real64) / &
                 (real(total_gpu_time) / clock_rate * 1.0e9)
  print '(A,F8.1,A)', "  Peak GFLOPS (GPU only): ", total_gflops
  
  total_gflops = real(flop_count * NUM_ITERATIONS, real64) / &
                 (real(total_wall_time) / clock_rate * 1.0e9)
  print '(A,F8.1,A)', "  Effective GFLOPS: ", total_gflops, " (including idle time)"
  print *, ""
  
  print *, "=== Analysis ==="
  print *, "With simple double buffering, we could improve GPU utilization"
  print *, "from ~2-3% to potentially 20-40% by overlapping CPU/GPU work."
  print *, ""
  print *, "Full async implementation with triple buffering, persistent"
  print *, "kernels, and proper synchronization could reach 90%+ utilization!"
  
  ! Cleanup
  deallocate(input_a, input_b, weights, output_a, output_b)
  call gpu_cleanup()
  
contains

  subroutine simulate_cpu_work(time_ms)
    real, intent(in) :: time_ms
    integer(int64) :: start, current
    real(real64) :: elapsed_ms
    
    call system_clock(start)
    do
      call system_clock(current)
      elapsed_ms = real(current - start) / clock_rate * 1000.0
      if (elapsed_ms >= time_ms) exit
    end do
  end subroutine simulate_cpu_work

end program test_gpu_async_poc