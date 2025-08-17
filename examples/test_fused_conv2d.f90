program test_fused_conv2d
  use iso_fortran_env
  use cpu_conv2d_fused
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time_unfused, time_fused, max_diff
  integer :: i
  
  print *, "=== FUSED vs UNFUSED CONVOLUTION COMPARISON ==="
  print *, ""
  
  ! Test parameters - medium size that shows the difference
  N = 1
  C = 64
  H = 56
  W = 56
  K = 64
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Problem size: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output size: ", N, "x", K, "x", H_out, "x", W_out
  print '(A,I0,A,I0)', "Kernel: ", kernel_size, "x", kernel_size
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  output1 = 0.0
  output2 = 0.0
  
  ! Test 1: Original unfused approach (im2col → cold buffer → GEMM)
  print *, "Test 1: UNFUSED approach (im2col → cold buffer → GEMM)"
  time_unfused = fused_conv2d_cpu(input, weights, output1, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  
  ! Test 2: Fused approach (hot cache processing)
  print *, "Test 2: FUSED approach (im2col+GEMM with hot cache)"
  time_fused = conv2d_fused_hot_cache(input, weights, output2, &
                                      N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Verify results match
  max_diff = 0.0
  do i = 1, size(output1)
    max_diff = max(max_diff, abs(output1(i) - output2(i)))
  end do
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Unfused time: ", time_unfused, " ms"
  print '(A,F10.2,A)', "Fused time: ", time_fused, " ms"
  print '(A,F10.2,A)', "Speedup: ", time_unfused / time_fused, "x"
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-5) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ!"
  end if
  
  ! Test different sizes
  print *, ""
  print *, "=== TESTING DIFFERENT PROBLEM SIZES ==="
  
  ! Small problem
  print *, ""
  print *, "Small problem (fits in L2):"
  N = 1; C = 3; H = 32; W = 32; K = 32
  call test_size(N, C, H, W, K, kernel_size, stride, pad)
  
  ! Large problem  
  print *, ""
  print *, "Large problem (memory-bound):"
  N = 1; C = 256; H = 28; W = 28; K = 512
  call test_size(N, C, H, W, K, kernel_size, stride, pad)
  
  deallocate(input, weights, output1, output2)
  
contains

  subroutine test_size(N, C, H, W, K, kernel_size, stride, pad)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad
    integer :: H_out, W_out
    real(real32) :: time1, time2
    
    H_out = (H + 2*pad - kernel_size) / stride + 1
    W_out = (W + 2*pad - kernel_size) / stride + 1
    
    if (allocated(input)) deallocate(input)
    if (allocated(weights)) deallocate(weights)
    if (allocated(output1)) deallocate(output1)
    if (allocated(output2)) deallocate(output2)
    
    allocate(input(N * C * H * W))
    allocate(weights(K * C * kernel_size * kernel_size))
    allocate(output1(N * K * H_out * W_out))
    allocate(output2(N * K * H_out * W_out))
    
    call random_number(input)
    call random_number(weights)
    
    time1 = fused_conv2d_cpu(input, weights, output1, &
                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    time2 = conv2d_fused_hot_cache(input, weights, output2, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    print '(A,F8.2,A,F8.2,A,F5.2,A)', &
      "  Unfused: ", time1, " ms, Fused: ", time2, " ms, Speedup: ", time1/time2, "x"
    
  end subroutine test_size
  
end program test_fused_conv2d