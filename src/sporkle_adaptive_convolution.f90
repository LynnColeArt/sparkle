module sporkle_adaptive_convolution
  ! ========================================================================
  ! Adaptive Convolution Orchestrator for Sporkle
  ! 
  ! This module brings together:
  ! 1. Adaptive parameter passing (UNIFORM/BUFFER/INLINE)
  ! 2. Adaptive device orchestration (CPU/iGPU/dGPU)
  ! 3. Fortran DSL for GPU kernels
  ! 
  ! Goal: Achieve 14 TFLOPS like we did on Metal, but with more flexibility
  ! ========================================================================
  
  use kinds
  use iso_c_binding
  use sporkle_types
  use sporkle_fortran_params
  use sporkle_shader_parser_v2
  use sporkle_fortran_shaders_v2
  use sporkle_mesh_types
  use sporkle_scheduler
  
  implicit none
  private
  
  public :: adaptive_conv2d, convolution_profile
  public :: im2col_kernel, gemm_kernel
  
  ! Convolution profiling data
  type :: convolution_profile
    ! Input dimensions
    integer :: batch_size
    integer :: height, width
    integer :: channels
    integer :: kernel_h, kernel_w
    integer :: stride_h, stride_w
    integer :: pad_h, pad_w
    
    ! Performance characteristics
    real :: arithmetic_intensity  ! FLOPs per byte
    integer(i64) :: total_flops
    integer(i64) :: memory_bytes
    
    ! Device suitability scores
    real :: cpu_score
    real :: igpu_score  
    real :: dgpu_score
    
    ! Selected strategies
    integer :: param_method      ! PARAMS_UNIFORM/BUFFER/INLINE
    integer :: primary_device    ! Which GPU to use
    logical :: use_multi_gpu     ! Split across devices?
  end type
  
contains

  ! ========================================================================
  ! Main entry point: Adaptive 2D convolution
  ! ========================================================================
  subroutine adaptive_conv2d(input, kernel, output, profile)
    real(sp), intent(in) :: input(:,:,:,:)   ! NCHW
    real(sp), intent(in) :: kernel(:,:,:,:)  ! OCHW  
    real(sp), intent(out) :: output(:,:,:,:) ! NCHW
    type(convolution_profile), intent(inout) :: profile
    
    type(param_strategy) :: param_strat
    type(shader_kernel_v2) :: im2col_kernel_obj, gemm_kernel_obj
    character(len=:), allocatable :: im2col_glsl, gemm_glsl
    integer :: selected_device
    
    ! Step 1: Profile the convolution
    call profile_convolution(input, kernel, output, profile)
    
    ! Step 2: Benchmark parameter passing methods
    param_strat = create_param_strategy()
    call benchmark_for_convolution(param_strat, profile)
    profile%param_method = param_strat%preferred_method
    
    ! Step 3: Select optimal device(s)
    call select_convolution_device(profile, selected_device)
    profile%primary_device = selected_device
    
    ! Step 4: Generate optimized kernels
    im2col_kernel_obj = parse_fortran_kernel_v2( &
      "kernels_convolution.f90", "im2col_nhwc", profile%param_method)
    gemm_kernel_obj = parse_fortran_kernel_v2( &
      "kernels_convolution.f90", "gemm_tiled", profile%param_method)
    
    im2col_glsl = generate_glsl_v2(im2col_kernel_obj)
    gemm_glsl = generate_glsl_v2(gemm_kernel_obj)
    
    ! Step 5: Execute with selected strategies
    print *, "=== Adaptive Convolution Execution Plan ==="
    print *, "Parameter method: ", get_method_name(profile%param_method)
    print *, "Primary device: ", get_device_name(profile%primary_device)
    print *, "Arithmetic intensity: ", profile%arithmetic_intensity
    print *, ""
    
    ! TODO: Actually execute on selected device
    ! For now, this is where we'd dispatch to the appropriate backend
    
  end subroutine adaptive_conv2d
  
  ! ========================================================================
  ! Profile convolution characteristics
  ! ========================================================================
  subroutine profile_convolution(input, kernel, output, profile)
    real(sp), intent(in) :: input(:,:,:,:)
    real(sp), intent(in) :: kernel(:,:,:,:)
    real(sp), intent(out) :: output(:,:,:,:)
    type(convolution_profile), intent(inout) :: profile
    
    integer :: N, C, H, W, K, R, S, P, Q
    integer(i64) :: gemm_m, gemm_n, gemm_k
    
    ! Extract dimensions
    N = size(input, 1)   ! Batch
    C = size(input, 2)   ! Input channels
    H = size(input, 3)   ! Height
    W = size(input, 4)   ! Width
    K = size(kernel, 1)  ! Output channels
    R = size(kernel, 3)  ! Kernel height
    S = size(kernel, 4)  ! Kernel width
    
    ! Output dimensions (assuming stride=1, pad=0 for now)
    P = H - R + 1
    Q = W - S + 1
    
    profile%batch_size = N
    profile%height = H
    profile%width = W
    profile%channels = C
    profile%kernel_h = R
    profile%kernel_w = S
    
    ! GEMM dimensions after im2col
    gemm_m = K
    gemm_n = N * P * Q
    gemm_k = C * R * S
    
    ! Calculate FLOPs and memory
    profile%total_flops = 2 * gemm_m * gemm_n * gemm_k
    profile%memory_bytes = 4 * (N*C*H*W + K*C*R*S + N*K*P*Q)  ! float32
    profile%arithmetic_intensity = real(profile%total_flops) / real(profile%memory_bytes)
    
    ! Score devices based on characteristics
    call score_devices_for_conv(profile)
    
  end subroutine profile_convolution
  
  ! ========================================================================
  ! Score devices for this specific convolution
  ! ========================================================================
  subroutine score_devices_for_conv(profile)
    type(convolution_profile), intent(inout) :: profile
    
    ! CPU: Good for small kernels, low arithmetic intensity
    if (profile%kernel_h <= 3 .and. profile%kernel_w <= 3) then
      profile%cpu_score = 0.8
    else
      profile%cpu_score = 0.3
    end if
    
    ! iGPU: Good for memory-bound operations (low arithmetic intensity)
    if (profile%arithmetic_intensity < 10.0) then
      profile%igpu_score = 0.9  ! Close to memory
    else
      profile%igpu_score = 0.6
    end if
    
    ! dGPU: Best for compute-bound operations (high arithmetic intensity)
    if (profile%arithmetic_intensity > 20.0) then
      profile%dgpu_score = 1.0  ! Maximum compute
    else
      profile%dgpu_score = 0.7
    end if
    
    ! Adjust for problem size
    if (profile%total_flops < 1000000) then  ! < 1 MFLOP
      profile%cpu_score = profile%cpu_score * 1.2
      profile%dgpu_score = profile%dgpu_score * 0.5  ! Overhead not worth it
    end if
    
  end subroutine score_devices_for_conv
  
  ! ========================================================================
  ! Benchmark parameter methods specifically for convolution
  ! ========================================================================
  subroutine benchmark_for_convolution(strat, profile)
    type(param_strategy), intent(inout) :: strat
    type(convolution_profile), intent(in) :: profile
    
    character(len=1024) :: dummy_kernel
    
    ! Create a dummy kernel that mimics convolution parameter usage
    write(dummy_kernel, '(A)') &
      "void main() {" // NEW_LINE('A') // &
      "  // Convolution parameters" // NEW_LINE('A') // &
      "  uint batch = params.batch;" // NEW_LINE('A') // &
      "  uint height = params.height;" // NEW_LINE('A') // &
      "  uint width = params.width;" // NEW_LINE('A') // &
      "  // Simulate work" // NEW_LINE('A') // &
      "}"
    
    ! Benchmark with convolution-like parameters
    call benchmark_param_methods(strat, dummy_kernel)
    
    ! Adjust scores based on convolution profile
    if (profile%kernel_h * profile%kernel_w > 9) then
      ! Many parameters favor BUFFER method
      strat%methods(PARAMS_BUFFER)%dispatch_time_ms = &
        strat%methods(PARAMS_BUFFER)%dispatch_time_ms * 0.8
    end if
    
    ! Recompute preferred method
    call select_preferred_method(strat)
    
  end subroutine benchmark_for_convolution
  
  ! ========================================================================
  ! Select optimal device for convolution
  ! ========================================================================
  subroutine select_convolution_device(profile, device_id)
    type(convolution_profile), intent(in) :: profile
    integer, intent(out) :: device_id
    
    real :: max_score
    
    ! Simple selection for now
    max_score = profile%cpu_score
    device_id = 0  ! CPU
    
    if (profile%igpu_score > max_score) then
      max_score = profile%igpu_score
      device_id = 1  ! iGPU (renderD129)
    end if
    
    if (profile%dgpu_score > max_score) then
      max_score = profile%dgpu_score
      device_id = 2  ! dGPU (renderD128)
    end if
    
    ! Check if we should use multiple GPUs
    if (profile%batch_size > 32 .and. &
        profile%dgpu_score > 0.8 .and. &
        profile%igpu_score > 0.7) then
      profile%use_multi_gpu = .true.
    else
      profile%use_multi_gpu = .false.
    end if
    
  end subroutine select_convolution_device
  
  ! ========================================================================
  ! Utility functions
  ! ========================================================================
  function get_method_name(method) result(name)
    integer, intent(in) :: method
    character(len=32) :: name
    
    select case(method)
    case(PARAMS_UNIFORM)
      name = "UNIFORM"
    case(PARAMS_BUFFER)
      name = "BUFFER"
    case(PARAMS_INLINE)
      name = "INLINE"
    case default
      name = "UNKNOWN"
    end select
  end function
  
  function get_device_name(device_id) result(name)
    integer, intent(in) :: device_id
    character(len=32) :: name
    
    select case(device_id)
    case(0)
      name = "CPU"
    case(1)
      name = "AMD Raphael iGPU"
    case(2)
      name = "AMD Radeon RX 7900 XT"
    case default
      name = "Unknown Device"
    end select
  end function
  
  subroutine select_preferred_method(strat)
    type(param_strategy), intent(inout) :: strat
    integer :: i
    real :: min_time, total_time
    
    min_time = huge(1.0)
    strat%preferred_method = PARAMS_BUFFER  ! Safe default
    
    do i = 1, 3
      if (strat%methods(i)%is_supported) then
        total_time = strat%methods(i)%setup_time_ms + &
                    strat%methods(i)%dispatch_time_ms
        if (total_time < min_time) then
          min_time = total_time
          strat%preferred_method = i
        end if
      end if
    end do
  end subroutine

end module sporkle_adaptive_convolution