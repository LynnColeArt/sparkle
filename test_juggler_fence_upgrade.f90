program test_juggler_fence_upgrade
  ! Test: Juggler Fence Upgrade Performance
  ! ======================================
  !
  ! Compares original glFinish-based juggling vs fence-based juggling
  
  use kinds
  use iso_c_binding
  use sporkle_conv2d_juggling, only: conv2d_auto_juggling, init_juggling_system, &
                                    cleanup_juggling_system
  use sporkle_conv2d_juggling_fence, only: conv2d_auto_juggling_fence, &
                                          init_juggling_system_fence, &
                                          cleanup_juggling_system_fence
  implicit none
  
  ! Test parameters
  integer, parameter :: N = 8        ! Batch size
  integer, parameter :: C = 64       ! Input channels  
  integer, parameter :: H = 56       ! Height
  integer, parameter :: W = 56       ! Width
  integer, parameter :: K = 128      ! Output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = H    ! Same due to padding
  integer, parameter :: W_out = W
  
  ! Data arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  real(sp), allocatable :: output_ref(:)
  
  ! Timing
  real(sp) :: time_original, time_fence
  real(sp) :: speedup
  integer :: i
  
  ! Performance stats
  integer(i64) :: total_flops
  real(sp) :: gflops_original, gflops_fence
  
  print *, "⚡ Juggler Fence Upgrade Test"
  print *, "============================"
  print *, ""
  
  ! Calculate total FLOPs
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A)', "Workload: ", total_flops / 1000000, " MFLOPS"
  print '(A,I0,A,I0,A,I0,A,I0)', "Dimensions: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0)', "Conv: ", K, " filters, ", kernel_size, "x", kernel_size
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  allocate(output_ref(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  input = input - 0.5
  weights = weights - 0.5
  
  ! Test 1: Original juggling (glFinish)
  print *, "🔧 Testing original juggling (glFinish)..."
  call init_juggling_system()
  
  ! Warm up
  time_original = conv2d_auto_juggling(input, weights, output_ref, &
                                      N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Measure
  time_original = 0.0
  do i = 1, 5
    time_original = time_original + conv2d_auto_juggling(input, weights, output_ref, &
                                                        N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  end do
  time_original = time_original / 5.0
  
  gflops_original = real(total_flops) / (time_original * 1.0e6)
  
  call cleanup_juggling_system()
  
  ! Test 2: Fence-based juggling
  print *, ""
  print *, "⚡ Testing fence-based juggling..."
  call init_juggling_system_fence()
  
  ! Warm up
  time_fence = conv2d_auto_juggling_fence(input, weights, output, &
                                         N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Measure
  time_fence = 0.0
  do i = 1, 5
    time_fence = time_fence + conv2d_auto_juggling_fence(input, weights, output, &
                                                        N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  end do
  time_fence = time_fence / 5.0
  
  gflops_fence = real(total_flops) / (time_fence * 1.0e6)
  
  call cleanup_juggling_system_fence()
  
  ! Calculate speedup
  speedup = time_original / time_fence
  
  ! Verify correctness
  print *, ""
  print *, "🔍 Verifying correctness..."
  block
    real(sp) :: max_diff, avg_diff
    integer :: j
    
    max_diff = 0.0
    avg_diff = 0.0
    do j = 1, size(output)
      max_diff = max(max_diff, abs(output(j) - output_ref(j)))
      avg_diff = avg_diff + abs(output(j) - output_ref(j))
    end do
    avg_diff = avg_diff / size(output)
    
    print '(A,E12.5)', "Max difference: ", max_diff
    print '(A,E12.5)', "Avg difference: ", avg_diff
    
    if (max_diff < 1e-4) then
      print *, "✅ Results match!"
    else
      print *, "❌ Results differ!"
    end if
  end block
  
  ! Results
  print *, ""
  print *, "📊 Performance Results"
  print *, "===================="
  print '(A,F8.2,A,F8.1,A)', "Original (glFinish):  ", time_original, " ms (", gflops_original, " GFLOPS)"
  print '(A,F8.2,A,F8.1,A)', "Fence-based:          ", time_fence, " ms (", gflops_fence, " GFLOPS)"
  print '(A,F8.2,A)', "Speedup:              ", speedup, "x"
  print *, ""
  
  ! Analysis
  if (speedup > 1.2) then
    print *, "✅ Significant speedup achieved!"
    print '(A,F6.1,A)', "   You save ", time_original - time_fence, " ms per operation"
    print '(A,F6.1,A)', "   At 100 ops/sec, that's ", (time_original - time_fence) * 100.0, " ms saved"
  else if (speedup > 1.0) then
    print *, "✅ Modest speedup achieved"
    print *, "   The benefit will be larger with more frequent syncs"
  else
    print *, "⚠️  No significant speedup"
    print *, "   This might be due to:"
    print *, "   - Large kernel execution time dominating sync overhead"
    print *, "   - Driver optimizations for glFinish"
    print *, "   - Need for more fine-grained synchronization points"
  end if
  
  ! Recommendations
  print *, ""
  print *, "💡 Recommendations:"
  print *, "   - Use fences for frequent small operations"
  print *, "   - Keep fence objects alive across frames"
  print *, "   - Consider timeout-based recovery for robustness"
  
  ! Cleanup
  deallocate(input, weights, output, output_ref)
  
end program test_juggler_fence_upgrade