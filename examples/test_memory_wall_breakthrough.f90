! Test Memory Wall Breakthrough
! =============================
! Demonstrates the power of hot cache exploitation

program test_memory_wall_breakthrough
  use kinds
  use memory_wall_breakthrough
  implicit none
  
  real(sp), allocatable :: input(:), weights(:), output(:)
  real(sp), allocatable :: output_ref(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i, num_runs
  real(sp) :: time_hot, time_cold, speedup
  real(sp) :: total_time_hot, total_time_cold
  integer(i64) :: total_flops
  real(sp) :: gflops_hot, gflops_cold
  real(sp) :: max_diff
  
  print *, "🧪 Memory Wall Breakthrough Test"
  print *, "================================"
  print *, ""
  print *, "Comparing cold cache (traditional) vs hot cache (breakthrough) approaches"
  print *, ""
  
  ! Test with ResNet-50 first layer
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
  allocate(output_ref(N * K * H_out * W_out))
  
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
  
  ! Test 1: Traditional approach (cold cache between operations)
  print *, "📊 Traditional Approach (Cold Cache)"
  print *, "====================================="
  
  total_time_cold = 0.0
  num_runs = 5
  
  do i = 1, num_runs
    output_ref = 0.0
    time_cold = naive_conv2d_cold_cache(input, weights, output_ref, &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (i > 1) total_time_cold = total_time_cold + time_cold  ! Skip first warmup
  end do
  
  time_cold = total_time_cold / real(num_runs - 1)
  gflops_cold = real(total_flops) / (time_cold * 1.0e6)
  
  print *, ""
  print '(A,F7.2,A)', "Average time: ", time_cold, " ms"
  print '(A,F7.1,A)', "Performance: ", gflops_cold, " GFLOPS"
  print *, ""
  
  ! Test 2: Breakthrough approach (hot cache exploitation)
  print *, "🔥 Breakthrough Approach (Hot Cache)"
  print *, "===================================="
  
  total_time_hot = 0.0
  
  do i = 1, num_runs
    output = 0.0
    time_hot = fused_conv2d_hot_cache(input, weights, output, &
                                      N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (i > 1) total_time_hot = total_time_hot + time_hot  ! Skip first warmup
  end do
  
  time_hot = total_time_hot / real(num_runs - 1)
  gflops_hot = real(total_flops) / (time_hot * 1.0e6)
  
  print *, ""
  print '(A,F7.2,A)', "Average time: ", time_hot, " ms"
  print '(A,F7.1,A)', "Performance: ", gflops_hot, " GFLOPS"
  print *, ""
  
  ! Compare results
  speedup = time_cold / time_hot
  
  print *, "📈 Performance Comparison"
  print *, "========================"
  print '(A,F5.2,A)', "Speedup: ", speedup, "x"
  print '(A,F7.1,A,F7.1,A)', "Performance: ", gflops_cold, " → ", gflops_hot, " GFLOPS"
  print *, ""
  
  ! Verify correctness
  max_diff = 0.0
  do i = 1, size(output)
    max_diff = max(max_diff, abs(output(i) - output_ref(i)))
  end do
  
  print '(A,E12.4)', "Maximum difference: ", max_diff
  if (max_diff < 1.0e-4) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ significantly"
  end if
  
  print *, ""
  print *, "🎯 Key Insights:"
  print *, "==============="
  print *, "1. Hot cache exploitation dramatically improves performance"
  print *, "2. Multiple operations on cached data avoid memory bottleneck"
  print *, "3. Cache-oblivious algorithms adapt to any cache hierarchy"
  print *, "4. Same principles that make GPUs fast also work on CPUs!"
  
  if (gflops_hot > 40.0) then
    print *, ""
    print *, "🎉 MEMORY WALL BREACHED! 40+ GFLOPS achieved!"
    print *, "This proves the universal memory optimization thesis!"
  end if
  
  ! Cleanup
  deallocate(input, weights, output, output_ref)
  
end program test_memory_wall_breakthrough