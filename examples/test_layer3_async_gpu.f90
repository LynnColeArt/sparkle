program test_layer3_async_gpu
  use iso_fortran_env
  use iso_c_binding
  use gpu_opengl_interface
  use gpu_async_executor
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
  
  ! Test configuration
  integer, parameter :: NUM_ITERATIONS = 10
  integer, parameter :: WARMUP_ITERATIONS = 2
  
  ! Arrays
  real(real32), allocatable, target :: input(:), weights(:), output(:)
  real(real32), allocatable :: output_sync(:)
  integer :: input_size, weight_size, output_size
  
  ! GPU resources
  type(gpu_async_state) :: async_state
  integer :: compute_program, weight_buffer
  
  ! Timing
  real(real32) :: sync_time, async_time, total_sync_time, total_async_time
  real(real32) :: sync_gflops, async_gflops
  integer(8) :: total_flops
  integer :: i, set_id
  logical :: all_complete
  
  ! OpenGL constants (minimal set needed)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  integer(c_int), parameter :: GL_STATIC_DRAW = int(z'88E4', c_int)
  
  print *, "🔦 Layer 3: Async GPU Execution Test"
  print *, "===================================="
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  total_flops = int(N, 8) * K * H_out * W_out * C * kernel_size * kernel_size * 2
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  allocate(output_sync(output_size))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Get compute program from reference implementation
  compute_program = gpu_get_program_id()
  if (compute_program == 0) then
    print *, "❌ Failed to get compute program"
    stop 1
  end if
  
  print *, "✅ GPU initialized with compute program:", compute_program
  
  ! Create weight buffer (shared for all operations)
  block
    use, intrinsic :: iso_c_binding
    interface
      subroutine glGenBuffers(n, buffers) bind(C, name="glGenBuffers")
        import :: c_int
        integer(c_int), value :: n
        integer(c_int), intent(out) :: buffers
      end subroutine
      
      subroutine glBindBuffer(target, buffer) bind(C, name="glBindBuffer")
        import :: c_int
        integer(c_int), value :: target, buffer
      end subroutine
      
      subroutine glBufferData(target, size, data, usage) bind(C, name="glBufferData")
        import :: c_int, c_size_t, c_ptr
        integer(c_int), value :: target, usage
        integer(c_size_t), value :: size
        type(c_ptr), value :: data
      end subroutine
    end interface
    
    call glGenBuffers(1, weight_buffer)
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, weight_buffer)
    call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                     int(weight_size * 4, c_size_t), &
                     c_loc(weights), GL_STATIC_DRAW)
  end block
  
  ! Initialize async executor
  call gpu_async_executor_init(async_state, compute_program, weight_buffer)
  
  print *, ""
  print *, "📊 Test Configuration:"
  print '(A,I3,A,I3,A,I3,A,I3)', " Input: ", N, "x", C, "x", H, "x", W
  print '(A,I3,A,I3)', " Kernel: ", kernel_size, "x", kernel_size
  print '(A,I3,A,I3,A,I3,A,I3)', " Output: ", N, "x", K, "x", H_out, "x", W_out
  print *, " Iterations:", NUM_ITERATIONS
  print *, ""
  
  ! Test 1: Synchronous execution (baseline)
  print *, "🔄 Testing synchronous execution..."
  total_sync_time = 0.0
  
  do i = 1, NUM_ITERATIONS + WARMUP_ITERATIONS
    output_sync = 0.0
    sync_time = gpu_execute_conv2d_ref(input, weights, output_sync, &
                                      N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (i > WARMUP_ITERATIONS) then
      total_sync_time = total_sync_time + sync_time
    end if
  end do
  
  sync_gflops = real(total_flops * NUM_ITERATIONS) / (total_sync_time * 1.0e6)
  print '(A,F8.2,A,F8.1,A)', "   Average sync time: ", &
        total_sync_time / NUM_ITERATIONS, " ms (", sync_gflops, " GFLOPS)"
  
  ! Test 2: Asynchronous execution
  print *, ""
  print *, "⚡ Testing asynchronous execution..."
  total_async_time = 0.0
  
  ! Submit all work asynchronously
  block
    real(real64) :: start_time, end_time
    call cpu_time(start_time)
    
    do i = 1, NUM_ITERATIONS + WARMUP_ITERATIONS
      ! Get next available buffer set
      set_id = gpu_get_next_buffer_set(async_state)
      
      ! Submit work
      ! For now, calculate grid dimensions for the dispatch
      block
        integer :: total_outputs, local_size, num_groups
        total_outputs = N * K * H_out * W_out
        local_size = 64  ! Match shader workgroup size
        num_groups = (total_outputs + local_size - 1) / local_size
        
        call gpu_submit_work_async(async_state, set_id, &
                                  input, output, &
                                  int(input_size, 8), int(output_size, 8), &
                                  num_groups, 1, 1)
      end block
    end do
    
    ! Wait for all to complete
    all_complete = .false.
    do while (.not. all_complete)
      all_complete = .true.
      do i = 1, MAX_IN_FLIGHT
        if (gpu_is_work_complete(async_state, i)) then
          call gpu_wait_for_completion(async_state, i)
        else
          all_complete = .false.
        end if
      end do
    end do
    
    call cpu_time(end_time)
    total_async_time = real((end_time - start_time) * 1000.0) ! Convert to ms
  end block
  
  async_gflops = real(total_flops * (NUM_ITERATIONS + WARMUP_ITERATIONS)) / (total_async_time * 1.0e6)
  print '(A,F8.2,A,F8.1,A)', "   Total async time: ", &
        total_async_time, " ms (", async_gflops, " GFLOPS)"
  print '(A,F8.2,A)', "   Speedup: ", sync_gflops / async_gflops, "x"
  
  ! Cleanup
  call gpu_async_executor_cleanup(async_state)
  
  print *, ""
  print *, "📈 Performance Summary:"
  print '(A,F8.1,A)', "  Synchronous:  ", sync_gflops, " GFLOPS"
  print '(A,F8.1,A)', "  Asynchronous: ", async_gflops, " GFLOPS"
  print *, ""
  print *, "🔍 Analysis:"
  if (async_gflops > sync_gflops * 1.1) then
    print *, "  ✅ Async execution provides performance benefit!"
  else
    print *, "  ℹ️  Workload too small to benefit from async execution"
    print *, "     Try larger batches or more iterations"
  end if
  
  ! Cleanup
  call gpu_cleanup()
  deallocate(input, weights, output, output_sync)
  
end program test_layer3_async_gpu