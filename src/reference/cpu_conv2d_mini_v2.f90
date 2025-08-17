! CPU Convolution V2 - Mini's optimizations with better inner loop
module cpu_conv2d_mini_v2
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized_v2, only: gemm_simd_avx512_v2
  implicit none
  
  private
  public :: conv2d_mini_v2
  
  ! Tile size for column partitioning - larger for better efficiency
  integer, parameter :: TILE_N = 256
  
contains

  real(real32) function conv2d_mini_v2(input, weights, output, &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Use int64 for all index calculations
    integer(int64) :: I, N_total, num_tiles
    integer(int64) :: tile_start, tile_end, tile_cols
    integer(int64) :: j, i_idx, n_batch, ch, kh, kw
    integer(int64) :: h_out_loc, w_out_loc, h_in, w_in
    integer(int64) :: in_idx, out_idx, col_offset
    integer :: tile_idx
    
    ! Thread-local scratch arrays
    real(real32), allocatable :: B_thread(:), C_thread(:)
    
    call cpu_time(start_time)
    
    ! Calculate dimensions using int64
    I = int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * int(N, int64)
    N_total = int(H_out, int64) * int(W_out, int64)
    
    ! Initialize output to zero
    output = 0.0
    
    ! Calculate number of tiles
    num_tiles = (N_total + TILE_N - 1) / TILE_N
    
    !$omp parallel private(tile_idx, tile_start, tile_end, tile_cols, &
    !$omp&                B_thread, C_thread, j, i_idx, n_batch, ch, &
    !$omp&                kh, kw, h_out_loc, w_out_loc, h_in, w_in, &
    !$omp&                in_idx, out_idx, col_offset)
    
    ! Allocate thread-local scratch
    allocate(B_thread(I * TILE_N), source=0.0_real32)
    allocate(C_thread(K * TILE_N), source=0.0_real32)
    
    ! Static scheduling with chunk size = 1
    !$omp do schedule(static, 1)
    do tile_idx = 1, int(num_tiles, int32)
      ! Calculate tile boundaries
      tile_start = int(tile_idx - 1, int64) * TILE_N + 1
      tile_end = min(tile_start + TILE_N - 1, N_total)
      tile_cols = tile_end - tile_start + 1
      
      ! Clear buffers
      B_thread(1:I*tile_cols) = 0.0
      C_thread(1:K*tile_cols) = 0.0
      
      ! Pack B_thread with optimized loop order
      ! Column-major packing: B(I, tile_cols)
      col_offset = 0
      do j = 1, tile_cols
        ! Map to output position
        out_idx = tile_start + j - 1
        h_out_loc = (out_idx - 1) / int(W_out, int64) + 1
        w_out_loc = mod(out_idx - 1, int(W_out, int64)) + 1
        
        ! Pack this column - optimize for contiguous access
        i_idx = 0
        do n_batch = 1, N
          do ch = 1, C
            ! Base input offset for this channel
            in_idx = ((int(n_batch, int64)-1)*int(C, int64) + (int(ch, int64)-1))*int(H*W, int64)
            
            do kh = 1, kernel_size
              h_in = (h_out_loc - 1) * int(stride, int64) + int(kh, int64) - int(pad, int64)
              
              ! Check row bounds once
              if (h_in >= 1 .and. h_in <= int(H, int64)) then
                ! Vectorized inner loop over kernel width
                !$omp simd
                do kw = 1, kernel_size
                  w_in = (w_out_loc - 1) * int(stride, int64) + int(kw, int64) - int(pad, int64)
                  
                  if (w_in >= 1 .and. w_in <= int(W, int64)) then
                    B_thread(col_offset + i_idx + kw) = input(in_idx + (h_in-1)*int(W, int64) + w_in)
                  else
                    B_thread(col_offset + i_idx + kw) = 0.0
                  end if
                end do
              else
                ! Entire row out of bounds - vectorized zero fill
                !$omp simd
                do kw = 1, kernel_size
                  B_thread(col_offset + i_idx + kw) = 0.0
                end do
              end if
              i_idx = i_idx + kernel_size
            end do
          end do
        end do
        col_offset = col_offset + I
      end do
      
      ! High-performance GEMM on thread-local data
      call gemm_simd_avx512_v2(weights, B_thread(1:I*tile_cols), &
                              C_thread(1:K*tile_cols), &
                              K, int(tile_cols, int32), int(I, int32), &
                              1.0, 0.0, &
                              K, int(I, int32), K)
      
      ! Write results to exclusive output region
      col_offset = 0
      do j = 1, tile_cols
        out_idx = tile_start + j - 1
        h_out_loc = (out_idx - 1) / int(W_out, int64) + 1
        w_out_loc = mod(out_idx - 1, int(W_out, int64)) + 1
        
        ! Vectorized write of K channels
        !$omp simd
        do i_idx = 1, K
          out_idx = (int(i_idx, int64)-1)*int(H_out*W_out, int64) + &
                    (h_out_loc-1)*int(W_out, int64) + w_out_loc
          output(out_idx) = C_thread(col_offset + i_idx)
        end do
        col_offset = col_offset + K
      end do
    end do
    !$omp end do
    
    deallocate(B_thread, C_thread)
    !$omp end parallel
    
    call cpu_time(end_time)
    conv2d_mini_v2 = real(end_time - start_time, real32) * 1000.0
    
    ! Calculate performance
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    gflops = real(total_flops, real64) / (conv2d_mini_v2 * 1.0e6_real64)
    
    print *, "🎯 Mini V2 - Optimized Inner Loop"
    print '(A,I0,A)', "   Column tile: ", TILE_N, " locations"
    print '(A,I0,A,I0)', "   Matrix: ", K, "×", I, " * ", I, "×", N_total
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_mini_v2, " ms, ", gflops, " GFLOPS"
    print *, "   ✅ SIMD on inner kernel loops"
    print *, "   ✅ Thread-local scratch (no races)"
    print *, "   ✅ Optimized memory access patterns"
    
  end function conv2d_mini_v2
  
end module cpu_conv2d_mini_v2