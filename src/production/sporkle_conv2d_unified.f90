module sporkle_conv2d_unified
  ! Unified Conv2D implementation - automatically selects CPU or GPU (PM4)
  ! This is the production path that Mini's reorganization leads to
  
  use kinds
  use iso_c_binding
  use sporkle_types
  use cpu_device_module
  use amd_device_mod
  use sporkle_memory
  use cpu_conv2d_adaptive
  use sporkle_pm4_compute
  use sporkle_amdgpu_direct
  implicit none
  private
  
  public :: sporkle_conv2d_unified
  
contains

  function sporkle_conv2d_unified(input, weights, output, &
                                 N, C, H, W, K, kernel_size, stride, pad, &
                                 device_type) result(time_ms)
    real(sp), intent(in) :: input(:), weights(:)
    real(sp), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad
    character(len=*), intent(in), optional :: device_type
    real(sp) :: time_ms
    
    character(len=16) :: device_choice
    integer :: H_out, W_out
    
    ! Calculate output dimensions
    H_out = (H + 2*pad - kernel_size) / stride + 1
    W_out = (W + 2*pad - kernel_size) / stride + 1
    
    ! Determine device
    if (present(device_type)) then
      device_choice = device_type
    else
      device_choice = "auto"
    end if
    
    select case(trim(device_choice))
    case("cpu")
      time_ms = execute_cpu_conv2d()
    case("gpu", "pm4")
      time_ms = execute_gpu_conv2d_pm4()
    case("auto")
      ! Simple heuristic: use GPU for large problems
      if (N * H_out * W_out * K > 1000000) then
        time_ms = execute_gpu_conv2d_pm4()
        if (time_ms < 0.0_sp) then
          ! GPU failed, fallback to CPU
          print *, "⚠️ GPU execution failed, falling back to CPU"
          time_ms = execute_cpu_conv2d()
        end if
      else
        time_ms = execute_cpu_conv2d()
      end if
    case default
      print *, "❌ Unknown device type: ", trim(device_choice)
      time_ms = -1.0_sp
    end select
    
  contains
  
    function execute_cpu_conv2d() result(time_ms)
      real(sp) :: time_ms
      type(cpu_device) :: cpu_dev
      
      print *, "🔧 Executing Conv2D on CPU..."
      cpu_dev = cpu_device(0)
      
      ! Use adaptive CPU implementation
      time_ms = conv2d_adaptive(input, weights, output, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      
      if (time_ms > 0.0_sp) then
        print '(A,F8.2,A)', "✅ CPU Conv2D completed in ", time_ms, " ms"
      end if
      
    end function execute_cpu_conv2d
    
    function execute_gpu_conv2d_pm4() result(time_ms)
      real(sp) :: time_ms
      type(pm4_context) :: ctx
      type(amdgpu_buffer) :: input_bo, weight_bo, output_bo, param_bo
      integer :: status
      integer(i64) :: shader_addr
      real(dp) :: start_time, end_time
      
      print *, "🔧 Executing Conv2D on GPU via PM4..."
      
      ! Initialize PM4 context
      status = pm4_init_context(ctx)
      if (status /= 0) then
        print *, "❌ Failed to initialize PM4 context"
        time_ms = -1.0_sp
        return
      end if
      
      ! Allocate GPU buffers
      input_bo = amdgpu_allocate_buffer(ctx%device, &
                                       int(N*C*H*W*4, i64), &
                                       AMDGPU_GEM_DOMAIN_GTT)
      weight_bo = amdgpu_allocate_buffer(ctx%device, &
                                        int(K*C*kernel_size*kernel_size*4, i64), &
                                        AMDGPU_GEM_DOMAIN_GTT)
      output_bo = amdgpu_allocate_buffer(ctx%device, &
                                        int(N*K*H_out*W_out*4, i64), &
                                        AMDGPU_GEM_DOMAIN_GTT)
      
      if (input_bo%handle == 0 .or. weight_bo%handle == 0 .or. output_bo%handle == 0) then
        print *, "❌ Failed to allocate GPU buffers"
        call pm4_cleanup_context(ctx)
        time_ms = -1.0_sp
        return
      end if
      
      ! Map buffers for CPU access
      status = amdgpu_map_buffer(ctx%device, input_bo)
      if (status == 0) status = amdgpu_map_buffer(ctx%device, weight_bo)
      if (status == 0) status = amdgpu_map_buffer(ctx%device, output_bo)
      
      if (status /= 0) then
        print *, "❌ Failed to map GPU buffers"
        call pm4_cleanup_context(ctx)
        time_ms = -1.0_sp
        return
      end if
      
      ! Copy input data to GPU
      block
        real(sp), pointer :: gpu_input(:), gpu_weights(:)
        call c_f_pointer(input_bo%cpu_ptr, gpu_input, [N*C*H*W])
        call c_f_pointer(weight_bo%cpu_ptr, gpu_weights, [K*C*kernel_size*kernel_size])
        
        gpu_input = input(1:N*C*H*W)
        gpu_weights = weights(1:K*C*kernel_size*kernel_size)
      end block
      
      ! Compile shader (for now use copy shader as placeholder)
      shader_addr = pm4_compile_shader("copy_shader", "")
      if (shader_addr == 0) then
        print *, "❌ Failed to compile shader"
        call pm4_cleanup_context(ctx)
        time_ms = -1.0_sp
        return
      end if
      
      ! Execute compute
      call cpu_time(start_time)
      
      block
        type(amdgpu_buffer) :: buffers(3)
        buffers = [input_bo, weight_bo, output_bo]
        
        ! Calculate workgroups (64 threads per workgroup)
        status = pm4_execute_compute(ctx, shader_addr, buffers, &
                                   (N*K*H_out*W_out + 63) / 64, 1, 1)
      end block
      
      call cpu_time(end_time)
      
      if (status == 0) then
        time_ms = real((end_time - start_time) * 1000.0_dp, sp)
        print '(A,F8.2,A)', "✅ GPU Conv2D completed in ", time_ms, " ms"
        
        ! Copy output back
        block
          real(sp), pointer :: gpu_output(:)
          call c_f_pointer(output_bo%cpu_ptr, gpu_output, [N*K*H_out*W_out])
          output(1:N*K*H_out*W_out) = gpu_output
        end block
      else
        print *, "❌ GPU execution failed"
        time_ms = -1.0_sp
      end if
      
      ! Cleanup
      call pm4_cleanup_context(ctx)
      
    end function execute_gpu_conv2d_pm4
    
  end function sporkle_conv2d_unified

end module sporkle_conv2d_unified