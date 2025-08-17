! Workload Profiler - Analyzes task characteristics for intelligent device scheduling
! This is the brain that decides: CPU or GPU? Small or large? Memory-bound or compute-bound?

module sparkle_workload_profiler
  use iso_fortran_env, only: real32, int32, int64
  implicit none
  
  private
  public :: workload_profile, analyze_workload, print_workload_analysis
  
  ! Workload characteristics
  type :: workload_profile
    ! Basic dimensions
    integer(int64) :: total_flops          ! Total floating point operations
    integer(int64) :: memory_accesses      ! Total memory reads/writes
    integer :: parallelism_factor          ! How parallel is this workload?
    
    ! Derived characteristics
    real(real32) :: arithmetic_intensity   ! FLOPS per byte accessed
    character(len=16) :: size_category     ! "tiny", "small", "medium", "large", "huge"
    character(len=16) :: compute_pattern   ! "elementwise", "reduction", "convolution", "matmul"
    character(len=16) :: memory_pattern    ! "sequential", "strided", "random", "broadcast"
    
    ! Device preferences (0.0 = avoid, 1.0 = perfect)
    real(real32) :: cpu_suitability       ! How suitable for CPU
    real(real32) :: gpu_suitability       ! How suitable for GPU
    real(real32) :: neural_suitability    ! How suitable for Neural Engine
    
    ! Performance estimates
    real(real32) :: estimated_cpu_time_ms ! Expected CPU time
    real(real32) :: estimated_gpu_time_ms ! Expected GPU time
    real(real32) :: transfer_overhead_ms  ! GPU transfer cost
    
    ! Scheduling decision
    character(len=16) :: recommended_device ! "cpu", "gpu", "neural", "hybrid"
    real(real32) :: confidence            ! How confident are we? (0.0-1.0)
  end type
  
contains

  ! Analyze a convolution workload
  function analyze_workload(N, C, H, W, K, kernel_size, stride, pad) result(profile)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad
    type(workload_profile) :: profile
    
    integer :: H_out, W_out
    integer(int64) :: input_size, weight_size, output_size
    real(real32) :: problem_scale
    
    ! Calculate output dimensions
    H_out = (H + 2*pad - kernel_size) / stride + 1
    W_out = (W + 2*pad - kernel_size) / stride + 1
    
    ! Basic metrics
    profile%total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                         int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    input_size = int(N, int64) * int(C, int64) * int(H, int64) * int(W, int64)
    weight_size = int(K, int64) * int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64)
    output_size = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64)
    
    profile%memory_accesses = input_size + weight_size + output_size
    profile%arithmetic_intensity = real(profile%total_flops) / real(profile%memory_accesses * 4)  ! 4 bytes per float
    
    ! Parallelism analysis
    profile%parallelism_factor = N * K * H_out * W_out  ! Independent output elements
    
    ! Size categorization
    problem_scale = real(profile%total_flops) / 1.0e9  ! GFLOPs
    if (problem_scale < 0.01) then
      profile%size_category = "tiny"
    else if (problem_scale < 0.1) then
      profile%size_category = "small"
    else if (problem_scale < 1.0) then
      profile%size_category = "medium"
    else if (problem_scale < 10.0) then
      profile%size_category = "large"
    else
      profile%size_category = "huge"
    end if
    
    ! Pattern recognition
    profile%compute_pattern = "convolution"
    if (kernel_size == 1) then
      profile%memory_pattern = "sequential"
    else
      profile%memory_pattern = "strided"
    end if
    
    ! Device suitability analysis
    call analyze_cpu_suitability(profile)
    call analyze_gpu_suitability(profile)
    call analyze_neural_suitability(profile)
    
    ! Performance estimates (heuristics based on our measurements)
    call estimate_performance(profile)
    
    ! Make scheduling decision
    call make_scheduling_decision(profile)
    
  end function analyze_workload
  
  subroutine analyze_cpu_suitability(profile)
    type(workload_profile), intent(inout) :: profile
    
    profile%cpu_suitability = 1.0  ! Start optimistic
    
    ! CPU is great for small problems
    select case(profile%size_category)
    case("tiny", "small")
      profile%cpu_suitability = profile%cpu_suitability * 1.2  ! Boost for small
    case("medium")
      profile%cpu_suitability = profile%cpu_suitability * 1.0  ! Neutral
    case("large", "huge")
      profile%cpu_suitability = profile%cpu_suitability * 0.7  ! Penalty for large
    end select
    
    ! CPU handles low parallelism well
    if (profile%parallelism_factor < 1000) then
      profile%cpu_suitability = profile%cpu_suitability * 1.3  ! Boost for low parallelism
    end if
    
    ! CPU is good for high arithmetic intensity (cache-friendly)
    if (profile%arithmetic_intensity > 10.0) then
      profile%cpu_suitability = profile%cpu_suitability * 1.2
    end if
    
    ! Clamp to reasonable range
    profile%cpu_suitability = min(1.0, max(0.1, profile%cpu_suitability))
  end subroutine
  
  subroutine analyze_gpu_suitability(profile)
    type(workload_profile), intent(inout) :: profile
    
    profile%gpu_suitability = 1.0  ! Start optimistic
    
    ! GPU loves large problems
    select case(profile%size_category)
    case("tiny", "small")
      profile%gpu_suitability = profile%gpu_suitability * 0.3  ! Major penalty for small
    case("medium")
      profile%gpu_suitability = profile%gpu_suitability * 0.8  ! Some penalty
    case("large", "huge")
      profile%gpu_suitability = profile%gpu_suitability * 1.5  ! Major boost for large
    end select
    
    ! GPU needs high parallelism
    if (profile%parallelism_factor > 10000) then
      profile%gpu_suitability = profile%gpu_suitability * 1.4  ! Boost for high parallelism
    else if (profile%parallelism_factor < 1000) then
      profile%gpu_suitability = profile%gpu_suitability * 0.5  ! Penalty for low parallelism
    end if
    
    ! GPU prefers compute-bound workloads
    if (profile%arithmetic_intensity > 5.0) then
      profile%gpu_suitability = profile%gpu_suitability * 1.2
    else if (profile%arithmetic_intensity < 1.0) then
      profile%gpu_suitability = profile%gpu_suitability * 0.7  ! Memory-bound penalty
    end if
    
    ! Clamp to reasonable range
    profile%gpu_suitability = min(1.0, max(0.1, profile%gpu_suitability))
  end subroutine
  
  subroutine analyze_neural_suitability(profile)
    type(workload_profile), intent(inout) :: profile
    
    ! Neural Engine is specialized for specific convolution patterns
    profile%neural_suitability = 0.5  ! Start conservative
    
    ! Neural Engine loves specific convolution sizes
    if (profile%compute_pattern == "convolution") then
      ! Boost for typical neural network layers
      if (profile%size_category == "medium" .or. profile%size_category == "large") then
        profile%neural_suitability = 0.9
      end if
    end if
    
    ! TODO: Add more sophisticated Neural Engine analysis when we have Apple hardware
  end subroutine
  
  subroutine estimate_performance(profile)
    type(workload_profile), intent(inout) :: profile
    
    real(real32) :: base_cpu_gflops, base_gpu_gflops
    real(real32) :: cpu_efficiency, gpu_efficiency
    
    ! Base performance estimates from our measurements
    base_cpu_gflops = 25.0   ! Our parallel CPU implementation
    base_gpu_gflops = 450.0  ! Our GPU implementation
    
    ! Efficiency factors based on workload characteristics
    cpu_efficiency = profile%cpu_suitability
    gpu_efficiency = profile%gpu_suitability
    
    ! Estimate CPU time
    profile%estimated_cpu_time_ms = real(profile%total_flops) / &
                                   (base_cpu_gflops * cpu_efficiency * 1.0e6)
    
    ! Estimate GPU time (including transfer overhead)
    profile%estimated_gpu_time_ms = real(profile%total_flops) / &
                                   (base_gpu_gflops * gpu_efficiency * 1.0e6)
    
    ! Transfer overhead (heuristic: ~1ms base + size-dependent)
    profile%transfer_overhead_ms = 1.0 + real(profile%memory_accesses) / 1.0e8
    
    ! Add transfer overhead to GPU time
    profile%estimated_gpu_time_ms = profile%estimated_gpu_time_ms + profile%transfer_overhead_ms
  end subroutine
  
  subroutine make_scheduling_decision(profile)
    type(workload_profile), intent(inout) :: profile
    
    real(real32) :: cpu_score, gpu_score, neural_score
    real(real32) :: best_score
    
    ! Score each device (lower time = higher score)
    cpu_score = 1.0 / max(0.1, profile%estimated_cpu_time_ms)
    gpu_score = 1.0 / max(0.1, profile%estimated_gpu_time_ms) 
    neural_score = profile%neural_suitability * 0.5  ! Conservative for now
    
    ! Find the best option
    best_score = max(cpu_score, gpu_score, neural_score)
    
    if (cpu_score == best_score) then
      profile%recommended_device = "cpu"
      profile%confidence = profile%cpu_suitability
    else if (gpu_score == best_score) then
      profile%recommended_device = "gpu"  
      profile%confidence = profile%gpu_suitability
    else
      profile%recommended_device = "neural"
      profile%confidence = profile%neural_suitability
    end if
    
    ! Consider hybrid approaches for borderline cases
    if (abs(cpu_score - gpu_score) / max(cpu_score, gpu_score) < 0.2) then
      profile%recommended_device = "hybrid"
      profile%confidence = 0.5  ! Lower confidence for borderline cases
    end if
    
  end subroutine
  
  subroutine print_workload_analysis(profile)
    type(workload_profile), intent(in) :: profile
    
    print *, ""
    print *, "🧠 Workload Analysis"
    print *, "==================="
    print '(A,I0,A)', "  Total FLOPs: ", profile%total_flops, ""
    print '(A,F6.2)', "  Arithmetic Intensity: ", profile%arithmetic_intensity
    print '(A,A)', "  Size Category: ", trim(profile%size_category)
    print '(A,A)', "  Compute Pattern: ", trim(profile%compute_pattern)
    print *, ""
    print *, "📊 Device Suitability:"
    print '(A,F5.2)', "  CPU Suitability: ", profile%cpu_suitability
    print '(A,F5.2)', "  GPU Suitability: ", profile%gpu_suitability  
    print '(A,F5.2)', "  Neural Suitability: ", profile%neural_suitability
    print *, ""
    print *, "⏱️  Performance Estimates:"
    print '(A,F6.2,A)', "  Estimated CPU Time: ", profile%estimated_cpu_time_ms, " ms"
    print '(A,F6.2,A)', "  Estimated GPU Time: ", profile%estimated_gpu_time_ms, " ms"
    print '(A,F6.2,A)', "  Transfer Overhead: ", profile%transfer_overhead_ms, " ms"
    print *, ""
    print *, "🎯 Scheduling Decision:"
    print '(A,A)', "  Recommended Device: ", trim(profile%recommended_device)
    print '(A,F5.2)', "  Confidence: ", profile%confidence
    print *, ""
    
  end subroutine
  
end module sparkle_workload_profiler