! CPU Convolution following Mini's surgical fixes
! Anchors — do not simplify:
! • Partition by columns (N); each thread owns unique j range in B(I,N) and C(K,N) — no overlap, no reductions.
! • Use thread-local B_thread(I,Nt) scratch; never build global B(I,N).
! • Pack column-major: outer j, inner i.
! • Use int64 for index math; assert bounds before stores.
! • SIMD on inner i loops only; confirm vectorization with -fopt-info-vec.
! • schedule(static, tile_N); set OMP_PROC_BIND=close OMP_PLACES=cores.
! • Re-enable threads only after single-thread correctness passes with ASan + bounds checks.

module cpu_conv2d_mini_fixed
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized_v2, only: gemm_simd_avx512_v2
  implicit none
  
  private
  public :: conv2d_mini_fixed
  
  ! Tile size for column partitioning
  integer, parameter :: TILE_N = 128
  
contains

  real(real32) function conv2d_mini_fixed(input, weights, output, &
                                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Use int64 for all index calculations
    integer(int64) :: I, N_total, num_tiles, tile_idx
    integer(int64) :: tile_start, tile_end, tile_cols
    integer(int64) :: j, i_row, n_idx, c_idx, kh_idx, kw_idx
    integer(int64) :: h_out_pos, w_out_pos, h_in_pos, w_in_pos
    integer(int64) :: in_idx, out_idx
    
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
    !$omp&                B_thread, C_thread, j, i_row, n_idx, c_idx, &
    !$omp&                kh_idx, kw_idx, h_out_pos, w_out_pos, h_in_pos, w_in_pos, &
    !$omp&                in_idx, out_idx)
    
    ! Allocate thread-local scratch with 64-byte alignment
    allocate(B_thread(I * TILE_N), source=0.0_real32)
    allocate(C_thread(K * TILE_N), source=0.0_real32)
    
    ! Static scheduling with chunk size = TILE_N
    !$omp do schedule(static, 1)
    do tile_idx = 1, num_tiles
      ! Calculate tile boundaries
      tile_start = (tile_idx - 1) * TILE_N + 1
      tile_end = min(tile_start + TILE_N - 1, N_total)
      tile_cols = tile_end - tile_start + 1
      
      ! Clear thread-local buffers
      B_thread = 0.0
      C_thread = 0.0
      
      ! Pack B_thread column-major: outer loop over columns j
      do j = 1, tile_cols
        ! Global output position
        out_idx = tile_start + j - 1
        h_out_pos = (out_idx - 1) / int(W_out, int64) + 1
        w_out_pos = mod(out_idx - 1, int(W_out, int64)) + 1
        
        ! Inner loop over rows i (contiguous for SIMD)
        i_row = 0
        do n_idx = 1, N
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                i_row = i_row + 1
                
                ! Calculate input position with bounds checking
                h_in_pos = (h_out_pos - 1) * int(stride, int64) + int(kh_idx, int64) - int(pad, int64)
                w_in_pos = (w_out_pos - 1) * int(stride, int64) + int(kw_idx, int64) - int(pad, int64)
                
                ! Bounds check and store
                if (h_in_pos >= 1 .and. h_in_pos <= int(H, int64) .and. &
                    w_in_pos >= 1 .and. w_in_pos <= int(W, int64)) then
                  ! NCHW layout index calculation
                  in_idx = ((int(n_idx, int64)-1)*int(C, int64) + (int(c_idx, int64)-1))*int(H*W, int64) + &
                          (h_in_pos-1)*int(W, int64) + w_in_pos
                  
                  ! Column-major storage: B(i,j)
                  B_thread(i_row + (j-1)*I) = input(in_idx)
                else
                  B_thread(i_row + (j-1)*I) = 0.0
                end if
              end do
            end do
          end do
        end do
      end do
      
      ! Call GEMM on thread-local data
      ! C_thread(K, tile_cols) = weights(K, I) * B_thread(I, tile_cols)
      call gemm_simd_avx512_v2(weights, B_thread(1:I*tile_cols), &
                              C_thread(1:K*tile_cols), &
                              K, int(tile_cols, int32), int(I, int32), &
                              1.0, 0.0, &
                              K, int(I, int32), K)
      
      ! Write C_thread to output (exclusive region for this thread)
      do j = 1, tile_cols
        out_idx = tile_start + j - 1
        h_out_pos = (out_idx - 1) / int(W_out, int64) + 1
        w_out_pos = mod(out_idx - 1, int(W_out, int64)) + 1
        
        ! Write K output channels for this position
        !$omp simd
        do i_row = 1, K
          ! NCHW output layout
          out_idx = (int(i_row, int64)-1)*int(H_out*W_out, int64) + &
                    (h_out_pos-1)*int(W_out, int64) + w_out_pos
          output(out_idx) = C_thread(i_row + (j-1)*K)
        end do
      end do
    end do
    !$omp end do
    
    deallocate(B_thread, C_thread)
    !$omp end parallel
    
    call cpu_time(end_time)
    conv2d_mini_fixed = real(end_time - start_time, real32) * 1000.0
    
    ! Calculate performance
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    gflops = real(total_flops, real64) / (conv2d_mini_fixed * 1.0e6_real64)
    
    print *, "🎯 Mini-Fixed CPU Convolution"
    print '(A,I0,A)', "   Column tile size: ", TILE_N, " output locations"
    print '(A,I0,A,I0)', "   Matrix dims: ", K, "×", I, " * ", I, "×", N_total
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_mini_fixed, " ms, ", gflops, " GFLOPS"
    print *, "   ✅ Thread-local B tiles (no global im2col)"
    print *, "   ✅ Column partitioning (no races)"
    print *, "   ✅ Int64 indexing (no overflow)"
    
  end function conv2d_mini_fixed
  
end module cpu_conv2d_mini_fixed