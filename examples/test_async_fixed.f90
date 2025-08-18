program test_async_fixed
  use iso_fortran_env, only: real32, int64
  use gpu_async_executor_fixed
  use sparkle_opengl_context
  use sparkle_conv2d_reference
  implicit none
  
  type(gpu_async_state) :: async_state
  integer :: width, height, compute_program, weight_buffer
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32), allocatable :: input(:), weights(:), output(:)
  real(real32), allocatable :: batch_outputs(:,:)
  integer :: i, batch, num_batches
  integer :: set_ids(10)
  logical :: all_done
  integer(int64) :: start_time, end_time, rate
  real :: total_time, throughput
  
  ! Initialize OpenGL
  call sparkle_init_gl(width, height)
  
  ! Create compute program
  compute_program = create_compute_shader()
  
  ! Test parameters
  N = 1
  C = 64
  H = 56
  W = 56
  K = 128
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Allocate data
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize test data
  call random_number(input)
  call random_number(weights)
  
  ! Create weight buffer
  weight_buffer = create_weight_buffer(weights)
  
  ! Initialize async executor
  call gpu_async_init(async_state, compute_program, weight_buffer, max_sets=5_c_int)
  
  print *, "🧪 Testing Fixed Async Executor"
  print *, "================================"
  print *, ""
  
  ! Test 1: Single synchronous operation
  print *, "Test 1: Single synchronous operation"
  call system_clock(start_time, rate)
  
  call gpu_conv2d_sync(async_state, input, output, &
                      int(size(input)*4, c_size_t), &
                      int(size(output)*4, c_size_t), &
                      (W_out + 15) / 16, (H_out + 15) / 16, N * K)
  
  call system_clock(end_time)
  total_time = real(end_time - start_time) / real(rate)
  print '(A,F6.2,A)', "   Time: ", total_time * 1000.0, " ms"
  print *, ""
  
  ! Test 2: True async operations
  print *, "Test 2: Multiple async operations"
  num_batches = 10
  allocate(batch_outputs(size(output), num_batches))
  
  call system_clock(start_time)
  
  ! Submit all batches asynchronously
  do batch = 1, num_batches
    ! Vary input slightly for each batch
    input = input * (1.0 + 0.01 * batch)
    
    set_ids(batch) = gpu_submit_conv2d(async_state, input, batch_outputs(:,batch), &
                                       int(size(input)*4, c_size_t), &
                                       int(size(output)*4, c_size_t), &
                                       (W_out + 15) / 16, (H_out + 15) / 16, N * K)
    
    print '(A,I2,A,I0)', "   Submitted batch ", batch, " to set ", set_ids(batch)
  end do
  
  ! Poll for completion
  print *, "   Polling for completion..."
  all_done = .false.
  do while (.not. all_done)
    all_done = .true.
    do batch = 1, num_batches
      if (set_ids(batch) > 0) then
        if (gpu_poll_complete(async_state, set_ids(batch))) then
          ! Gather this result
          call gpu_gather_output(async_state, set_ids(batch), &
                               batch_outputs(:,batch), &
                               int(size(output)*4, c_size_t))
          print '(A,I2)', "   ✅ Batch completed: ", batch
          set_ids(batch) = -1  ! Mark as collected
        else
          all_done = .false.
        end if
      end if
    end do
  end do
  
  call system_clock(end_time)
  total_time = real(end_time - start_time) / real(rate)
  throughput = real(num_batches) / total_time
  
  print '(A,F6.2,A)', "   Total time: ", total_time * 1000.0, " ms"
  print '(A,F6.2,A)', "   Throughput: ", throughput, " batches/sec"
  print *, ""
  
  ! Test 3: Demonstrate oldest-first selection
  print *, "Test 3: Oldest-first fence selection"
  print *, "   Submitting 3 operations with delays..."
  
  ! Submit first (will be oldest)
  set_ids(1) = gpu_submit_conv2d(async_state, input, output, &
                                int(size(input)*4, c_size_t), &
                                int(size(output)*4, c_size_t), &
                                (W_out + 15) / 16, (H_out + 15) / 16, N * K)
  print '(A,I0)', "   Op 1 -> set ", set_ids(1)
  
  ! Small delay
  call sleep_ms(10)
  
  ! Submit second
  set_ids(2) = gpu_submit_conv2d(async_state, input, output, &
                                int(size(input)*4, c_size_t), &
                                int(size(output)*4, c_size_t), &
                                (W_out + 15) / 16, (H_out + 15) / 16, N * K)
  print '(A,I0)', "   Op 2 -> set ", set_ids(2)
  
  ! Submit third
  set_ids(3) = gpu_submit_conv2d(async_state, input, output, &
                                int(size(input)*4, c_size_t), &
                                int(size(output)*4, c_size_t), &
                                (W_out + 15) / 16, (H_out + 15) / 16, N * K)
  print '(A,I0)', "   Op 3 -> set ", set_ids(3)
  
  ! Now submit 4th - should reuse oldest completed
  set_ids(4) = gpu_submit_conv2d(async_state, input, output, &
                                int(size(input)*4, c_size_t), &
                                int(size(output)*4, c_size_t), &
                                (W_out + 15) / 16, (H_out + 15) / 16, N * K)
  print '(A,I0,A)', "   Op 4 -> set ", set_ids(4), " (should reuse oldest)"
  
  ! Gather all
  do i = 1, 4
    if (set_ids(i) > 0) then
      call gpu_gather_output(async_state, set_ids(i), output, &
                           int(size(output)*4, c_size_t))
    end if
  end do
  
  ! Cleanup
  call gpu_async_cleanup(async_state)
  call sparkle_cleanup_gl()
  
  print *, ""
  print *, "✅ All tests complete!"
  
contains

  subroutine sleep_ms(ms)
    integer, intent(in) :: ms
    integer(int64) :: start, now, rate
    real :: elapsed
    
    call system_clock(start, rate)
    do
      call system_clock(now)
      elapsed = real(now - start) / real(rate) * 1000.0
      if (elapsed >= ms) exit
    end do
  end subroutine

  function create_compute_shader() result(program)
    integer :: program
    ! Simplified - would load real shader
    program = 1
  end function
  
  function create_weight_buffer(weights) result(buffer)
    real(real32), intent(in) :: weights(:)
    integer :: buffer
    ! Simplified - would create real buffer
    buffer = 2
  end function

end program test_async_fixed