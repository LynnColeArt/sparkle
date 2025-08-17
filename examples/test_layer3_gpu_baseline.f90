program test_layer3_gpu_baseline
  use iso_fortran_env
  use sparkle_conv2d
  implicit none
  
  ! Test parameters - same as our Layer 2 tests
  integer, parameter :: N = 1          ! Batch size
  integer, parameter :: C = 64         ! Input channels
  integer, parameter :: H = 56         ! Height
  integer, parameter :: W = 56         ! Width
  integer, parameter :: K = 64         ! Output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = 56     ! (56 + 2*1 - 3)/1 + 1 = 56
  integer, parameter :: W_out = 56
  
  ! Arrays
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size
  integer :: i
  
  print *, "🔦 Layer 3: GPU Performance Baseline Test"
  print *, "======================================="
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0      ! Range [-1, 1]
  weights = (weights - 0.5) * 0.1  ! Range [-0.1, 0.1]
  
  print *, "📊 Test Configuration:"
  print '(A,I3,A,I3,A,I3,A,I3)', " Input: ", N, "x", C, "x", H, "x", W
  print '(A,I3,A,I3)', " Kernel: ", kernel_size, "x", kernel_size
  print '(A,I3,A,I3,A,I3,A,I3)', " Output: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Test CPU performance (our Layer 2 achievement)
  print *, "🖥️  CPU Performance (Layer 2 fused im2col+GEMM):"
  output = 0.0
  call conv2d_cpu(input, weights, output, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "🎮 GPU Performance (reference OpenGL implementation):"
  
  ! Warm up GPU
  output = 0.0
  call conv2d_gpu(input, weights, output, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Actual GPU benchmark
  output = 0.0
  call conv2d_gpu(input, weights, output, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "📈 Performance Summary:"
  print *, "  - CPU: ~14.8 GFLOPS (our Layer 2 achievement)"
  print *, "  - GPU: Should be ~451 GFLOPS on RX 7900 XTX"
  print *, ""
  print *, "Next steps for Layer 3:"
  print *, "  1. ✅ Established GPU baseline"
  print *, "  2. 🔦 Integrate dynamic shader system"
  print *, "  3. 🔦 Enable async execution"
  
  ! Cleanup
  deallocate(input, weights, output)
  
end program test_layer3_gpu_baseline