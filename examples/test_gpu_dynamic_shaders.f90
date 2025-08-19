program test_gpu_dynamic_shaders
  use iso_c_binding
  use gpu_opengl_interface
  use gpu_dynamic_execution
  use sporkle_types
  use kinds
  implicit none
  
  ! Test parameters (ResNet-50 first layer)
  integer, parameter :: N = 1, C = 3, H = 224, W = 224
  integer, parameter :: K = 64, kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = 112, W_out = 112
  
  ! Arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  
  print *, "=== GPU Dynamic Shader Execution Test ==="
  print *, ""
  print *, "This test will actually execute different shader variants on the GPU"
  print *, "and measure their real performance."
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "ERROR: Failed to initialize GPU!"
    stop 1
  end if
  
  ! Allocate arrays
  allocate(input(N*C*H*W))
  allocate(weights(K*C*kernel_size*kernel_size))
  allocate(output(N*K*H_out*W_out))
  
  ! Initialize test data
  call random_number(input)
  call random_number(weights)
  
  ! Run the benchmark
  call gpu_benchmark_shader_variants(input, weights, output, &
                                    N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "Key insights:"
  print *, "- If all variants show ~417 GFLOPS, the shader differences don't matter for this workload"
  print *, "- If dual-issue shows improvement, RDNA3 optimization is working"
  print *, "- If larger workgroups show degradation, wave scheduling matters"
  
  ! Cleanup
  call gpu_cleanup()
  deallocate(input, weights, output)
  
end program test_gpu_dynamic_shaders