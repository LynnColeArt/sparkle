program test_fp16_accuracy
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use cpu_conv2d_fused_fp16
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:)
  real(real32), allocatable :: output_ref(:), output_fp32(:), output_fp16(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time_ref, time_fp32, time_fp16
  real(real32) :: max_diff_fp32, max_diff_fp16
  real(real32) :: rel_err_fp32, rel_err_fp16
  integer :: total_flops
  
  print *, "=== FP16 vs FP32 ACCURACY COMPARISON ==="
  print *, ""
  
  ! Production-like size
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
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Test size: ", N, "x", C, "x", H, "x", W
  print '(A,I0)', "Output elements: ", N * K * H_out * W_out
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output_ref(N * K * H_out * W_out))
  allocate(output_fp32(N * K * H_out * W_out))
  allocate(output_fp16(N * K * H_out * W_out))
  
  ! Initialize with values that might show precision differences
  call random_number(input)
  call random_number(weights)
  
  ! Scale to larger range to show FP16 limitations
  input = (input - 0.5) * 10.0
  weights = (weights - 0.5) * 2.0
  
  print *, ""
  print *, "=== RUNNING COMPUTATIONS ==="
  
  ! Reference (unfused FP32)
  output_ref = 0.0
  time_ref = fused_conv2d_cpu(input, weights, output_ref, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Fused FP32
  output_fp32 = 0.0
  time_fp32 = conv2d_fused_correct(input, weights, output_fp32, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Fused FP16
  output_fp16 = 0.0
  time_fp16 = conv2d_fused_fp16(input, weights, output_fp16, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "=== PERFORMANCE ==="
  total_flops = N * K * H_out * W_out * C * kernel_size * kernel_size * 2
  print '(A,F10.2,A,F8.1,A)', "Reference FP32: ", time_ref, " ms (", &
    real(total_flops) / (time_ref * 1.0e6), " GFLOPS)"
  print '(A,F10.2,A,F8.1,A)', "Fused FP32:     ", time_fp32, " ms (", &
    real(total_flops) / (time_fp32 * 1.0e6), " GFLOPS)"
  print '(A,F10.2,A,F8.1,A)', "Fused FP16:     ", time_fp16, " ms (", &
    real(total_flops) / (time_fp16 * 1.0e6), " GFLOPS)"
  
  ! Accuracy analysis
  max_diff_fp32 = maxval(abs(output_ref - output_fp32))
  max_diff_fp16 = maxval(abs(output_ref - output_fp16))
  
  rel_err_fp32 = max_diff_fp32 / maxval(abs(output_ref))
  rel_err_fp16 = max_diff_fp16 / maxval(abs(output_ref))
  
  print *, ""
  print *, "=== ACCURACY vs REFERENCE ==="
  print '(A,E12.5,A,F8.5,A)', "FP32 fused: max_diff = ", max_diff_fp32, &
    " (", rel_err_fp32 * 100.0, "%)"
  print '(A,E12.5,A,F8.5,A)', "FP16 fused: max_diff = ", max_diff_fp16, &
    " (", rel_err_fp16 * 100.0, "%)"
  
  print *, ""
  print *, "=== STATISTICAL ANALYSIS ==="
  call print_error_stats("FP32 fused", output_ref, output_fp32)
  call print_error_stats("FP16 fused", output_ref, output_fp16)
  
  print *, ""
  print *, "=== CONCLUSION ==="
  if (rel_err_fp16 < 0.01) then  ! 1% relative error
    print *, "✅ FP16 accuracy is acceptable for inference (<1% error)"
    print *, "✅ Expected ~2x speedup with real FP16 hardware"
  else if (rel_err_fp16 < 0.05) then  ! 5% relative error  
    print *, "⚠️  FP16 accuracy marginal for inference (1-5% error)"
    print *, "   Consider using bfloat16 or mixed precision"
  else
    print *, "❌ FP16 accuracy too poor for inference (>5% error)"
    print *, "   Need to investigate numerical stability"
  end if
  
  deallocate(input, weights, output_ref, output_fp32, output_fp16)
  
contains

  subroutine print_error_stats(label, ref, test)
    character(len=*), intent(in) :: label
    real(real32), intent(in) :: ref(:), test(:)
    
    real(real32) :: errors(size(ref))
    real(real32) :: mean_err, std_err, percentile_90, percentile_99
    integer :: n
    
    errors = abs(ref - test)
    n = size(errors)
    
    mean_err = sum(errors) / n
    std_err = sqrt(sum((errors - mean_err)**2) / n)
    
    ! Sort for percentiles
    call sort_array(errors)
    percentile_90 = errors(int(0.90 * n))
    percentile_99 = errors(int(0.99 * n))
    
    print '(A,A)', label, ":"
    print '(A,E12.5)', "  Mean absolute error: ", mean_err
    print '(A,E12.5)', "  Std deviation:       ", std_err
    print '(A,E12.5)', "  90th percentile:     ", percentile_90
    print '(A,E12.5)', "  99th percentile:     ", percentile_99
    
  end subroutine print_error_stats
  
  subroutine sort_array(arr)
    real(real32), intent(inout) :: arr(:)
    integer :: i, j, n
    real(real32) :: temp
    
    n = size(arr)
    do i = 1, n-1
      do j = i+1, n
        if (arr(i) > arr(j)) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
        end if
      end do
    end do
  end subroutine sort_array

end program test_fp16_accuracy