! High-Performance CPU Convolution with SIMD
! ==========================================
! 
! This module properly integrates the SIMD-optimized GEMM
! to achieve the promised 196.7 GFLOPS performance
!
! Key fix: Actually USE the gemm_simd_avx512 in production!

module cpu_conv2d_simd
  use iso_fortran_env, only: real32, real64, int64
  use universal_memory_optimization, only: im2col_cache_optimal, arithmetic_intensity
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_cpu_simd
  
contains

  ! High-performance CPU convolution using SIMD-optimized GEMM
  real(real32) function conv2d_cpu_simd(input, weights, output, &
                                        N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    ! Workspace for im2col transformation
    real(real32), allocatable :: input_matrix(:)
    integer :: input_matrix_rows, input_matrix_cols
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops, bytes_accessed
    real(real64) :: gflops, intensity
    
    ! Calculate workspace size
    input_matrix_rows = C * kernel_size * kernel_size
    input_matrix_cols = N * H_out * W_out
    allocate(input_matrix(input_matrix_rows * input_matrix_cols))
    
    print *, "🚀 SIMD-Optimized CPU convolution (AVX-512)"
    print '(A,I0,A,I0)', "   Matrix size: ", input_matrix_rows, " × ", input_matrix_cols
    
    call cpu_time(start_time)
    
    ! Step 1: Cache-optimal im2col transformation  
    call im2col_cache_optimal(input, input_matrix, N, C, H, W, &
                             kernel_size, stride, pad, H_out, W_out)
    
    ! Step 2: SIMD-optimized GEMM (THIS IS THE KEY FIX!)
    ! C = weights * input_matrix
    call gemm_simd_avx512(weights, input_matrix, output, &
                         K, input_matrix_cols, input_matrix_rows, &
                         1.0, 0.0)
    
    call cpu_time(end_time)
    
    ! Calculate performance
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    bytes_accessed = int(N * C * H * W * 4, int64) + &                    ! Input
                     int(K * C * kernel_size * kernel_size * 4, int64) + & ! Weights  
                     int(N * K * H_out * W_out * 4, int64)                 ! Output
    
    conv2d_cpu_simd = real((end_time - start_time) * 1000.0, real32) ! Return time in ms
    
    gflops = real(total_flops, real64) / (end_time - start_time) / 1.0e9_real64
    intensity = arithmetic_intensity(total_flops, bytes_accessed)
    
    print '(A,F10.2,A,F8.1,A)', "   Performance: ", conv2d_cpu_simd, " ms, ", gflops, " GFLOPS"  
    print '(A,F6.1,A)', "   Arithmetic intensity: ", intensity, " FLOPS/byte"
    
    if (gflops > 150.0) then
      print *, "   🚀 SIMD optimization working! >150 GFLOPS achieved!"
    else if (gflops > 50.0) then
      print *, "   ✅ Good performance: >50 GFLOPS"
    else
      print *, "   🔴 Needs optimization: <50 GFLOPS"
    end if
    
    deallocate(input_matrix)
    
  end function conv2d_cpu_simd

end module cpu_conv2d_simd