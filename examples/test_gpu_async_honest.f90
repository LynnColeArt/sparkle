program test_gpu_async_honest
  use kinds
  use iso_c_binding
  use gpu_opengl_interface
  use gpu_async_executor
  use gl_constants
  implicit none
  
  ! Test configuration
  integer, parameter :: NUM_BATCHES = 20
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
  real(sp), allocatable, target :: inputs(:,:)
  real(sp), allocatable, target :: weights(:)
  real(sp), allocatable, target :: outputs(:,:)
  
  ! Timing
  integer(i64) :: start_time, end_time, gpu_start, gpu_end
  real(dp) :: clock_rate
  real(sp) :: kernel_time_ms
  
  ! GPU resources
  integer :: compute_program, weight_buffer
  type(gpu_async_state) :: async_state
  
  ! Loop variables
  integer :: batch, set_id
  
  ! Sizes
  integer(i64) :: input_size, weight_size, output_size, flop_count
  real(dp) :: wall_time_ms, expected_time_ms, gflops
  
  print *, "=== GPU Async Honest Performance Test ==="
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  compute_program = gpu_get_program_id()
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate data
  allocate(inputs(input_size, NUM_BATCHES))
  allocate(weights(weight_size))
  allocate(outputs(output_size, NUM_BATCHES))
  
  ! Initialize with random data
  call random_number(inputs)
  call random_number(weights)
  
  ! Create weight buffer on GPU
  call glGenBuffers(1, weight_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, weight_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, weight_size * 4, c_loc(weights), GL_DYNAMIC_DRAW)
  
  ! Get clock rate
  call system_clock(count_rate=clock_rate)
  
  ! Calculate FLOPs
  flop_count = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
               int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2
  
  ! First, measure single kernel time for baseline
  print *, "Step 1: Measure Single Kernel Performance"
  print *, "-----------------------------------------"
  kernel_time_ms = gpu_execute_conv2d_ref(inputs(:, 1), weights, outputs(:, 1), &
                                         N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  print '(A,F8.2,A)', "Single kernel GPU time: ", kernel_time_ms, " ms"
  print '(A,F8.1,A)', "Single kernel GFLOPS: ", real(flop_count, real64) / (kernel_time_ms * 1.0e6), " GFLOPS"
  
  expected_time_ms = kernel_time_ms * NUM_BATCHES
  print '(A,I0,A,F8.2,A)', "Expected time for ", NUM_BATCHES, " batches: ", expected_time_ms, " ms"
  print *, ""
  
  ! Test async with proper measurement
  print *, "Step 2: Async Execution with Honest Timing"
  print *, "------------------------------------------"
  
  ! Initialize async executor
  call gpu_async_executor_init(async_state, compute_program, weight_buffer)
  
  ! Clear any pending work
  call glFinish()
  
  ! Start timing AFTER clearing pipeline
  call system_clock(start_time)
  
  ! Submit all work
  do batch = 1, NUM_BATCHES
    set_id = gpu_get_next_buffer_set(async_state)
    call gpu_submit_work_async(async_state, set_id, &
                              inputs(:, batch), outputs(:, batch), &
                              input_size * 4, output_size * 4, &
                              (W_out + 15) / 16, (H_out + 15) / 16, N * K)
  end do
  
  ! Wait for ALL work to complete
  call glFinish()
  
  ! Stop timing AFTER all work is done
  call system_clock(end_time)
  
  wall_time_ms = real(end_time - start_time) / clock_rate * 1000.0
  gflops = real(flop_count * NUM_BATCHES, real64) / (wall_time_ms * 1.0e6)
  
  print '(A,F8.2,A)', "Wall-clock time: ", wall_time_ms, " ms"
  print '(A,F8.1,A)', "Performance: ", gflops, " GFLOPS"
  print '(A,F8.2,A)', "Average per batch: ", wall_time_ms / NUM_BATCHES, " ms"
  print *, ""
  
  ! Analysis
  print *, "Step 3: Performance Analysis"
  print *, "----------------------------"
  print '(A,F8.2,A)', "GPU kernel time (expected): ", expected_time_ms, " ms"
  print '(A,F8.2,A)', "Actual wall time: ", wall_time_ms, " ms"
  print '(A,F5.2,A)', "Efficiency: ", (expected_time_ms / wall_time_ms) * 100.0, "%"
  
  if (wall_time_ms < expected_time_ms * 0.9) then
    print *, ""
    print *, "❌ ERROR: Wall time is less than GPU kernel time!"
    print *, "This violates physics - measurement methodology is flawed."
  else if (wall_time_ms < expected_time_ms * 1.1) then
    print *, ""
    print *, "✅ EXCELLENT: Near-perfect GPU utilization!"
    print *, "The async executor is keeping the GPU fully occupied."
  else
    print *, ""
    print *, "✓ GOOD: Async executor provides real speedup."
    print '(A,F5.2,A)', "Overhead: ", ((wall_time_ms / expected_time_ms) - 1.0) * 100.0, "%"
  end if
  
  ! Cleanup
  call gpu_async_executor_cleanup(async_state)
  call glDeleteBuffers(1, weight_buffer)
  deallocate(inputs, weights, outputs)
  call gpu_cleanup()

end program test_gpu_async_honest