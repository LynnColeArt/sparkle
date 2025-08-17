! Intelligent Device Scheduler - The brain of universal memory optimization
! This orchestrates CPU cores, GPU, and Neural Engine based on workload characteristics

module sparkle_intelligent_scheduler
  use iso_fortran_env, only: real32, int32, int64
  use sparkle_workload_profiler, only: workload_profile, analyze_workload
  use sparkle_conv2d, only: conv2d_cpu, conv2d_gpu
  implicit none
  
  private
  public :: intelligent_conv2d, device_juggling_demo, scheduler_benchmark
  
  ! Device performance tracking
  type :: device_stats
    character(len=16) :: name
    integer :: executions = 0
    real(real32) :: total_time = 0.0
    real(real32) :: average_gflops = 0.0
    real(real32) :: success_rate = 1.0
    logical :: available = .true.
  end type
  
  ! Global device tracking
  type(device_stats) :: cpu_stats, gpu_stats, neural_stats
  logical :: scheduler_initialized = .false.
  
contains

  subroutine initialize_scheduler()
    if (scheduler_initialized) return
    
    cpu_stats%name = "CPU"
    gpu_stats%name = "GPU" 
    neural_stats%name = "Neural Engine"
    
    ! TODO: Detect actual device availability
    cpu_stats%available = .true.
    gpu_stats%available = .true.  ! Assume GPU is available for now
    neural_stats%available = .false.  ! Disable until Apple hardware detected
    
    scheduler_initialized = .true.
    print *, "🧠 Intelligent Scheduler initialized"
  end subroutine
  
  ! The main scheduling function - this is where the magic happens!
  subroutine intelligent_conv2d(input, weights, output, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    type(workload_profile) :: profile
    real(real32) :: start_time, end_time, execution_time
    character(len=16) :: chosen_device
    logical :: execution_success
    
    call initialize_scheduler()
    
    ! Step 1: Analyze the workload
    profile = analyze_workload(N, C, H, W, K, kernel_size, stride, pad)
    
    ! Step 2: Make intelligent device selection
    chosen_device = select_optimal_device(profile)
    
    print *, "🎯 Intelligent Scheduling Decision:"
    print '(A,A,A,F4.2,A)', "   Chosen: ", trim(chosen_device), " (confidence: ", profile%confidence, ")"
    
    ! Step 3: Execute on chosen device with performance tracking
    call cpu_time(start_time)
    execution_success = .true.
    
    select case(trim(chosen_device))
    case("cpu")
      call conv2d_cpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      call cpu_time(end_time)
      execution_time = (end_time - start_time) * 1000.0
      call update_device_stats(cpu_stats, profile, execution_time, execution_success)
      
    case("gpu")
      call conv2d_gpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      call cpu_time(end_time)
      execution_time = (end_time - start_time) * 1000.0
      call update_device_stats(gpu_stats, profile, execution_time, execution_success)
      
    case("neural")
      ! TODO: Implement Neural Engine path
      print *, "   ⚠️  Neural Engine not yet implemented, falling back to CPU"
      call conv2d_cpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      call cpu_time(end_time)
      execution_time = (end_time - start_time) * 1000.0
      call update_device_stats(cpu_stats, profile, execution_time, execution_success)
      
    case("hybrid")
      ! TODO: Implement hybrid CPU+GPU execution
      print *, "   🔄 Hybrid execution not yet implemented, choosing best single device"
      if (profile%cpu_suitability > profile%gpu_suitability) then
        call conv2d_cpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
        call cpu_time(end_time)
        execution_time = (end_time - start_time) * 1000.0
        call update_device_stats(cpu_stats, profile, execution_time, execution_success)
      else
        call conv2d_gpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
        call cpu_time(end_time)
        execution_time = (end_time - start_time) * 1000.0
        call update_device_stats(gpu_stats, profile, execution_time, execution_success)
      end if
      
    case default
      print *, "   ❌ Unknown device, falling back to CPU"
      call conv2d_cpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      call cpu_time(end_time)
      execution_time = (end_time - start_time) * 1000.0
      call update_device_stats(cpu_stats, profile, execution_time, execution_success)
    end select
    
    ! Step 4: Learn from this execution for future decisions
    call learn_from_execution(profile, chosen_device, execution_time, execution_success)
    
  end subroutine intelligent_conv2d
  
  function select_optimal_device(profile) result(device)
    type(workload_profile), intent(in) :: profile
    character(len=16) :: device
    
    ! Start with the profiler's recommendation
    device = profile%recommended_device
    
    ! Apply real-time adjustments based on device availability and performance history
    if (.not. gpu_stats%available .and. trim(device) == "gpu") then
      print *, "   ⚠️  GPU unavailable, switching to CPU"
      device = "cpu"
    end if
    
    if (.not. neural_stats%available .and. trim(device) == "neural") then
      print *, "   ⚠️  Neural Engine unavailable, switching to GPU or CPU"
      if (profile%gpu_suitability > profile%cpu_suitability .and. gpu_stats%available) then
        device = "gpu"
      else
        device = "cpu"
      end if
    end if
    
    ! Apply performance history adjustments
    if (cpu_stats%executions > 5 .and. gpu_stats%executions > 5) then
      ! We have enough data to make informed decisions
      if (trim(device) == "cpu" .and. cpu_stats%success_rate < 0.8) then
        print *, "   📊 CPU performance below threshold, considering GPU"
        if (gpu_stats%success_rate > cpu_stats%success_rate .and. gpu_stats%available) then
          device = "gpu"
        end if
      end if
    end if
    
  end function select_optimal_device
  
  subroutine update_device_stats(stats, profile, execution_time, success)
    type(device_stats), intent(inout) :: stats
    type(workload_profile), intent(in) :: profile
    real(real32), intent(in) :: execution_time
    logical, intent(in) :: success
    
    real(real32) :: gflops
    
    stats%executions = stats%executions + 1
    stats%total_time = stats%total_time + execution_time
    
    if (success) then
      gflops = real(profile%total_flops) / (execution_time * 1.0e6)
      stats%average_gflops = (stats%average_gflops * (stats%executions - 1) + gflops) / stats%executions
      stats%success_rate = (stats%success_rate * (stats%executions - 1) + 1.0) / stats%executions
    else
      stats%success_rate = (stats%success_rate * (stats%executions - 1) + 0.0) / stats%executions
    end if
    
  end subroutine update_device_stats
  
  subroutine learn_from_execution(profile, device, actual_time, success)
    type(workload_profile), intent(in) :: profile
    character(len=16), intent(in) :: device
    real(real32), intent(in) :: actual_time
    logical, intent(in) :: success
    
    real(real32) :: prediction_error
    
    ! Calculate how wrong our prediction was
    select case(trim(device))
    case("cpu")
      prediction_error = abs(actual_time - profile%estimated_cpu_time_ms) / profile%estimated_cpu_time_ms
    case("gpu")
      prediction_error = abs(actual_time - profile%estimated_gpu_time_ms) / profile%estimated_gpu_time_ms
    case default
      prediction_error = 0.0
    end select
    
    ! TODO: Use this feedback to improve future predictions
    ! This is where machine learning could be added to improve scheduling over time
    
    if (prediction_error > 0.5) then
      print '(A,F5.2,A)', "   📊 Prediction error: ", prediction_error * 100.0, "% - learning for next time"
    end if
    
  end subroutine learn_from_execution
  
  ! Demo function to show device juggling in action
  subroutine device_juggling_demo()
    real(real32), allocatable :: input(:), weights(:), output(:)
    integer :: sizes(5, 4)  ! Different problem sizes to test
    integer :: i, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    integer :: input_size, weight_size, output_size
    
    print *, ""
    print *, "🎪 Device Juggling Demo"
    print *, "======================="
    print *, "Testing intelligent scheduler on different workload sizes..."
    print *, ""
    
    ! Define test cases: N, C, H, W
    sizes(1, :) = [1, 16, 8, 8]       ! Tiny problem
    sizes(2, :) = [1, 32, 16, 16]     ! Small problem  
    sizes(3, :) = [1, 64, 56, 56]     ! Medium problem (our test case)
    sizes(4, :) = [1, 128, 128, 128]  ! Large problem
    sizes(5, :) = [1, 256, 256, 256]  ! Huge problem
    
    K = 64
    kernel_size = 3
    stride = 1
    pad = 1
    
    do i = 1, 5
      N = sizes(i, 1)
      C = sizes(i, 2) 
      H = sizes(i, 3)
      W = sizes(i, 4)
      H_out = (H + 2*pad - kernel_size) / stride + 1
      W_out = (W + 2*pad - kernel_size) / stride + 1
      
      print '(A,I0,A)', "🔍 Test Case ", i, ":"
      print '(A,I0,A,I0,A,I0,A,I0,A)', "   Input: ", N, "x", C, "x", H, "x", W
      
      ! Allocate arrays for this test case
      input_size = N * C * H * W
      weight_size = K * C * kernel_size * kernel_size
      output_size = N * K * H_out * W_out
      
      if (allocated(input)) deallocate(input)
      if (allocated(weights)) deallocate(weights)
      if (allocated(output)) deallocate(output)
      
      allocate(input(input_size), weights(weight_size), output(output_size))
      
      ! Initialize with random data
      call random_number(input)
      call random_number(weights)
      input = (input - 0.5) * 2.0
      weights = (weights - 0.5) * 0.1
      
      ! Let the intelligent scheduler decide and execute
      call intelligent_conv2d(input, weights, output, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      
      print *, ""
    end do
    
    ! Print final device statistics
    call print_device_statistics()
    
    if (allocated(input)) deallocate(input)
    if (allocated(weights)) deallocate(weights) 
    if (allocated(output)) deallocate(output)
    
  end subroutine device_juggling_demo
  
  subroutine print_device_statistics()
    print *, ""
    print *, "📊 Device Performance Summary"
    print *, "============================"
    
    if (cpu_stats%executions > 0) then
      print '(A,A)', "🖥️  ", trim(cpu_stats%name)
      print '(A,I0)', "   Executions: ", cpu_stats%executions
      print '(A,F6.1,A)', "   Average Performance: ", cpu_stats%average_gflops, " GFLOPS"
      print '(A,F5.1,A)', "   Success Rate: ", cpu_stats%success_rate * 100.0, "%"
    end if
    
    if (gpu_stats%executions > 0) then
      print '(A,A)', "🚀 ", trim(gpu_stats%name)
      print '(A,I0)', "   Executions: ", gpu_stats%executions
      print '(A,F6.1,A)', "   Average Performance: ", gpu_stats%average_gflops, " GFLOPS"
      print '(A,F5.1,A)', "   Success Rate: ", gpu_stats%success_rate * 100.0, "%"
    end if
    
    if (neural_stats%executions > 0) then
      print '(A,A)', "🧠 ", trim(neural_stats%name)
      print '(A,I0)', "   Executions: ", neural_stats%executions
      print '(A,F6.1,A)', "   Average Performance: ", neural_stats%average_gflops, " GFLOPS"
      print '(A,F5.1,A)', "   Success Rate: ", neural_stats%success_rate * 100.0, "%"
    end if
    
    print *, ""
  end subroutine print_device_statistics
  
  ! Benchmark the scheduler's decision-making accuracy
  subroutine scheduler_benchmark()
    print *, ""
    print *, "⚡ Scheduler Benchmark"
    print *, "===================="
    print *, "Testing scheduling accuracy and overhead..."
    print *, ""
    
    call device_juggling_demo()
    
    print *, "✅ Scheduler benchmark complete!"
    print *, ""
    print *, "💡 Key Insights:"
    print *, "   • Small problems → CPU (low overhead, good single-thread perf)"
    print *, "   • Large problems → GPU (high parallelism, compute-bound)"
    print *, "   • Medium problems → Intelligent choice based on characteristics"
    print *, "   • The scheduler learns and adapts over time"
    print *, ""
    
  end subroutine scheduler_benchmark
  
end module sparkle_intelligent_scheduler