! Simple Memory Wall Breakthrough Test
! ====================================
! Focused test without dependencies

program test_memory_wall_simple
  use kinds
  use memory_wall_breakthrough, only: fused_conv2d_hot_cache, naive_conv2d_cold_cache
  implicit none
  
  real(sp), allocatable :: input(:), weights(:), output(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i
  real(sp) :: time_hot, time_cold, speedup
  integer(i64) :: total_flops
  real(sp) :: gflops_hot, gflops_cold
  
  print *, "🧪 Memory Wall Breakthrough Test (Simple)"
  print *, "========================================"
  print *, ""
  
  ! ResNet-50 first layer for better performance measurement
  N = 1
  C = 3  
  H = 224
  W = 224
  K = 64
  kernel_size = 7
  stride = 2
  pad = 3
  H_out = 112
  W_out = 112
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize test data
  do i = 1, size(input)
    input(i) = real(mod(i-1, 256)) / 256.0
  end do
  do i = 1, size(weights)
    weights(i) = real(mod(i-1, 128)) / 128.0 - 0.5
  end do
  
  ! Calculate total FLOPs
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "×", C, "×", H, "×", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output: ", N, "×", K, "×", H_out, "×", W_out
  print '(A,F0.1,A)', "Total GFLOPS: ", real(total_flops) / 1.0e9
  print *, ""
  
  ! Test 1: Naive approach (cold cache)
  print *, "📊 Naive Approach (Cold Cache)"
  print *, "=============================="
  output = 0.0
  time_cold = naive_conv2d_cold_cache(input, weights, output, &
                                     N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  gflops_cold = real(total_flops) / (time_cold * 1.0e6)
  print '(A,F7.2,A)', "Time: ", time_cold, " ms"
  print '(A,F7.1,A)', "Performance: ", gflops_cold, " GFLOPS"
  print *, ""
  
  ! Test 2: Hot cache exploitation
  print *, "🔥 Breakthrough Approach (Hot Cache)"
  print *, "===================================="
  output = 0.0
  time_hot = fused_conv2d_hot_cache(input, weights, output, &
                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  gflops_hot = real(total_flops) / (time_hot * 1.0e6)
  print '(A,F7.2,A)', "Time: ", time_hot, " ms"
  print '(A,F7.1,A)', "Performance: ", gflops_hot, " GFLOPS"
  print *, ""
  
  ! Compare results
  speedup = time_cold / time_hot
  
  print *, "📈 Performance Comparison"
  print *, "========================"
  print '(A,F5.2,A)', "Speedup: ", speedup, "x"
  print '(A,F7.1,A,F7.1,A)', "Performance: ", gflops_cold, " → ", gflops_hot, " GFLOPS"
  
  if (gflops_hot > 40.0) then
    print *, ""
    print *, "🎉 MEMORY WALL BREACHED! 40+ GFLOPS achieved!"
  end if
  
  ! Cleanup
  deallocate(input, weights, output)
  
end program test_memory_wall_simple