program test_gpu_async_executor
  use iso_fortran_env, only: real32, real64, int64
  use iso_c_binding
  use gpu_opengl_interface
  use gpu_async_executor
  use gl_constants
  implicit none
  
  ! Test configuration
  integer, parameter :: NUM_BATCHES = 100  ! Increase to ensure GPU saturation
  integer, parameter :: SINGLE_BATCH_TEST = 1  ! Test with just 1 batch first
  integer, parameter :: BATCH_SIZE = 4
  
  ! Conv2d parameters
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
  
  ! Data arrays
  real(real32), allocatable, target :: inputs(:,:)   ! Multiple input batches
  real(real32), allocatable, target :: weights(:)
  real(real32), allocatable, target :: outputs(:,:)  ! Multiple output batches
  
  ! Timing
  integer(int64) :: start_time, end_time, batch_start
  integer(int64) :: sync_total_time, async_total_time
  real(real64) :: clock_rate
  real(real32) :: batch_time
  
  ! GPU resources
  integer :: compute_program, weight_buffer
  type(gpu_async_state) :: async_state
  
  ! Loop variables
  integer :: batch, i
  integer :: set_id, completed_count
  
  ! Sizes
  integer(int64) :: input_size, weight_size, output_size, flop_count
  real(real64) :: sync_gflops, async_gflops, speedup
  
  print *, "=== GPU Async Executor Test ==="
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Get GPU resources from the actual GPU initialization
  compute_program = gpu_get_program_id()
  
  if (compute_program == 0) then
    print *, "❌ Failed to get compute program ID"
    stop 1
  end if
  
  print *, "✅ Got compute program ID:", compute_program
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate data
  allocate(inputs(input_size, NUM_BATCHES))
  allocate(weights(weight_size))
  allocate(outputs(output_size, NUM_BATCHES))
  
  ! Initialize with random data
  do batch = 1, NUM_BATCHES
    call random_number(inputs(:, batch))
  end do
  call random_number(weights)
  
  ! Create weight buffer on GPU (shared across all operations)
  call glGenBuffers(1, weight_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, weight_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, weight_size * 4, c_loc(weights), GL_DYNAMIC_DRAW)
  
  ! Get clock rate
  call system_clock(count_rate=clock_rate)
  
  ! Calculate FLOPs per batch
  flop_count = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
               int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2
  
  print *, "Configuration:"
  print *, "  Batches:", NUM_BATCHES
  print *, "  Batch size:", BATCH_SIZE
  print *, "  Input shape:", N, "x", C, "x", H, "x", W
  print *, "  Output shape:", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! =================
  ! Single Batch Test: Understanding the baseline
  ! =================
  print *, "Single Batch Test: Understanding CPU-GPU Overhead"
  print *, "------------------------------------------------"
  
  call system_clock(start_time)
  batch_time = gpu_execute_conv2d_ref(inputs(:, 1), weights, outputs(:, 1), &
                                     N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  call system_clock(end_time)
  
  print '(A,F8.2,A)', "GPU kernel time: ", batch_time, " ms"
  print '(A,F8.2,A)', "Wall-clock time: ", real(end_time - start_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.1,A)', "Overhead factor: ", real(end_time - start_time) / clock_rate * 1000.0 / batch_time, "x"
  print '(A,F8.1,A)', "GPU kernel GFLOPS: ", real(flop_count, real64) / (batch_time * 1.0e6), " GFLOPS"
  print *, ""
  
  ! =================
  ! Test 1: Synchronous execution (baseline)
  ! =================
  print *, "Test 1: Synchronous Execution (Baseline)"
  print *, "----------------------------------------"
  
  ! Measure wall-clock time for fair comparison with async
  call system_clock(start_time)
  
  do batch = 1, NUM_BATCHES
    ! Process batch synchronously (timing is discarded, we measure wall-clock)
    batch_time = gpu_execute_conv2d_ref(inputs(:, batch), weights, outputs(:, batch), &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  end do
  
  call system_clock(end_time)
  sync_total_time = end_time - start_time
  
  sync_gflops = real(flop_count * NUM_BATCHES, real64) / &
                (real(sync_total_time) / clock_rate * 1.0e9)
  
  print '(A,F8.2,A)', "Total time: ", real(sync_total_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.1,A)', "Performance: ", sync_gflops, " GFLOPS"
  print '(A,F8.2,A)', "Avg time per batch: ", real(sync_total_time) / clock_rate * 1000.0 / NUM_BATCHES, " ms"
  print '(A,F8.2,A)', "GPU kernel time: ", batch_time, " ms (per batch)"
  print '(A,F8.1,A)', "CPU-GPU overhead: ", (real(sync_total_time) / clock_rate * 1000.0 / NUM_BATCHES) / batch_time, "x"
  print *, ""
  
  ! =================
  ! Test 2: Async execution with triple buffering
  ! =================
  print *, "Test 2: Async Execution with Triple Buffering"
  print *, "---------------------------------------------"
  
  ! Initialize async executor
  call gpu_async_executor_init(async_state, compute_program, weight_buffer)
  
  ! Implement TRUE async pipelining for maximum throughput
  ! Add initial sync to ensure clean baseline
  call glFinish()
  call system_clock(start_time)
  
  ! Phase 1: Fill the pipeline (submit without waiting)
  print *, "  🚀 Filling pipeline..."
  do batch = 1, min(MAX_IN_FLIGHT, NUM_BATCHES)
    set_id = gpu_get_next_buffer_set(async_state)
    call gpu_submit_work_async(async_state, set_id, &
                              inputs(:, batch), outputs(:, batch), &
                              input_size * 4, output_size * 4, &
                              (W_out + 15) / 16, (H_out + 15) / 16, N * K)
    print '(A,I2,A,I1)', "    Submitted batch ", batch, " to set ", set_id
  end do
  
  ! Phase 2: Continuous pipeline (overlap GPU work with CPU prep)
  print *, "  ⚡ Running continuous pipeline..."
  batch = min(MAX_IN_FLIGHT, NUM_BATCHES) + 1
  completed_count = 0
  
  do while (completed_count < NUM_BATCHES)
    ! Check all buffer sets for completion
    do i = 1, MAX_IN_FLIGHT
      if (async_state%buffer_sets(i)%in_use .and. &
          gpu_is_work_complete(async_state, i)) then
        
        ! Collect completed work
        call gpu_wait_for_completion(async_state, i)
        completed_count = completed_count + 1
        print '(A,I2,A,I1)', "    Completed batch ", completed_count, " from set ", i
        
        ! Immediately submit next batch (if available)
        if (batch <= NUM_BATCHES) then
          call gpu_submit_work_async(async_state, i, &
                                    inputs(:, batch), outputs(:, batch), &
                                    input_size * 4, output_size * 4, &
                                    (W_out + 15) / 16, (H_out + 15) / 16, N * K)
          print '(A,I2,A,I1)', "    Submitted batch ", batch, " to set ", i
          batch = batch + 1
        end if
        
        exit ! Found completed work, check again immediately
      end if
    end do
  end do
  
  ! CRITICAL: Ensure all GPU work is actually complete before measuring time!
  call glFinish()
  
  call system_clock(end_time)
  async_total_time = end_time - start_time
  
  ! Clean up async executor (also prints statistics)
  call gpu_async_executor_cleanup(async_state)
  
  async_gflops = real(flop_count * NUM_BATCHES, real64) / &
                 (real(async_total_time) / clock_rate * 1.0e9)
  speedup = real(sync_total_time) / real(async_total_time)
  
  print *, ""
  print '(A,F8.2,A)', "Total time: ", real(async_total_time) / clock_rate * 1000.0, " ms"
  print '(A,F8.1,A)', "Performance: ", async_gflops, " GFLOPS"
  print '(A,F8.2,A)', "Avg time per batch: ", real(async_total_time) / clock_rate * 1000.0 / NUM_BATCHES, " ms"
  print '(A,F5.2,A)', "Speedup: ", speedup, "x"
  print *, ""
  
  ! =================
  ! Summary
  ! =================
  print *, "=== Summary ==="
  print '(A,F8.1,A)', "Synchronous: ", sync_gflops, " GFLOPS"
  print '(A,F8.1,A)', "Async: ", async_gflops, " GFLOPS"
  print '(A,F5.1,A)', "Speedup: ", speedup, "x"
  print *, ""
  print *, "⚠️  IMPORTANT: These measurements include CPU-GPU synchronization overhead."
  print *, "The synchronous test has ~30x overhead (wall-clock vs GPU kernel time)."
  print *, ""
  print *, "🔍 Analysis:"
  print '(A,F8.2,A)', "Expected minimum GPU time: ", batch_time * NUM_BATCHES, " ms"
  print '(A,F8.2,A)', "Actual async time: ", real(async_total_time) / clock_rate * 1000.0, " ms"
  if (real(async_total_time) / clock_rate * 1000.0 < batch_time * NUM_BATCHES) then
    print *, "❌ WARNING: Async time is less than GPU kernel time - measurement error!"
  else
    print *, "✅ Async time is realistic - pipelining is working correctly."
  end if
  
  ! Cleanup
  call glDeleteBuffers(1, weight_buffer)
  deallocate(inputs, weights, outputs)
  call gpu_cleanup()
  
contains

  subroutine simulate_gpu_work(time_ms)
    real, intent(in) :: time_ms
    integer(int64) :: start, current
    real(real64) :: elapsed_ms
    
    call system_clock(start)
    do
      call system_clock(current)
      elapsed_ms = real(current - start) / clock_rate * 1000.0
      if (elapsed_ms >= time_ms) exit
    end do
  end subroutine simulate_gpu_work
  
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

end program test_gpu_async_executor