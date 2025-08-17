program test_unified_device
  use iso_fortran_env, only: real32
  use sparkle_device_detection, only: detect_all_devices, get_cpu_info, get_gpu_info
  use sparkle_unified_device, only: unified_device, create_unified_device, &
                                    destroy_unified_device, execute_convolution
  implicit none
  
  type(unified_device) :: cpu_device, gpu_device
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: N=1, C=64, H=56, W=56, K=64, kernel_size=3
  integer :: stride=1, pad=1, H_out, W_out
  integer :: status
  real(real32) :: cpu_time, gpu_time
  
  print *, "🔧 Sparkle Unified Device Test"
  print *, "=============================="
  print *, ""
  
  ! Initialize device detection
  call detect_all_devices()
  
  ! Create unified devices
  print *, "Creating unified device interfaces..."
  cpu_device = create_unified_device(get_cpu_info())
  gpu_device = create_unified_device(get_gpu_info())
  
  ! Calculate dimensions
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Allocate test data
  allocate(input(N*C*H*W))
  allocate(weights(K*C*kernel_size*kernel_size))
  allocate(output(N*K*H_out*W_out))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  print *, ""
  print *, "🧪 Testing convolution on different devices:"
  print '(A,I0,A,I0,A,I0,A,I0)', "  Problem size: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0)', "  Output channels: ", K, ", Kernel: ", kernel_size
  print *, ""
  
  ! Test CPU execution
  if (cpu_device%available) then
    print *, "📊 CPU Execution:"
    call execute_convolution(cpu_device, input, weights, output, &
                            N, C, H, W, K, kernel_size, stride, pad, &
                            H_out, W_out, status=status)
    
    if (status == 0) then
      cpu_time = cpu_device%last_execution_time_ms
      print '(A,F8.2,A)', "  ✅ CPU completed in ", cpu_time, " ms"
      print '(A,I0)', "  Successful executions: ", cpu_device%successful_executions
    else
      print *, "  ❌ CPU execution failed"
    end if
  end if
  
  print *, ""
  
  ! Test GPU execution
  if (gpu_device%available) then
    print *, "🚀 GPU Execution:"
    call execute_convolution(gpu_device, input, weights, output, &
                            N, C, H, W, K, kernel_size, stride, pad, &
                            H_out, W_out, status=status)
    
    if (status == 0) then
      gpu_time = gpu_device%last_execution_time_ms
      print '(A,F8.2,A)', "  ✅ GPU completed in ", gpu_time, " ms"
      print '(A,I0)', "  Successful executions: ", gpu_device%successful_executions
    else
      print *, "  ❌ GPU execution failed"
    end if
  end if
  
  print *, ""
  print *, "📈 Performance Comparison:"
  if (cpu_device%available .and. gpu_device%available .and. &
      cpu_time > 0.0 .and. gpu_time > 0.0) then
    print '(A,F6.1,A)', "  Speedup: ", cpu_time / gpu_time, "x"
    print '(A,F8.2,A,F8.2,A)', "  CPU: ", cpu_time, " ms, GPU: ", gpu_time, " ms"
  end if
  
  ! Cleanup
  call destroy_unified_device(cpu_device)
  call destroy_unified_device(gpu_device)
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "✅ Unified device test complete!"

end program test_unified_device