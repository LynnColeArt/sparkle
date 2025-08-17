! Fused im2col+GEMM following Mini's recommendations - V2 with correct packing
! Column-major packing for Fortran!
module cpu_conv2d_fused_mini_v2
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized_v2, only: gemm_simd_avx512_v2
  implicit none
  
  private
  public :: conv2d_fused_mini_v2
  
contains

  real(real32) function conv2d_fused_mini_v2(input, weights, output, &
                                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    integer, parameter :: TILE_SIZE = 64
    real(real32), allocatable :: im2col_tile(:), output_tile(:)
    integer :: input_rows, total_output_locs
    
    integer :: tile_start, tile_end, tile_cols
    integer :: h_out_idx, w_out_idx, out_idx, out_loc
    integer :: n_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx
    integer :: num_tiles, tile_idx, k_idx
    integer :: linear_idx, output_offset
    integer :: row_idx, col_idx
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size * N
    total_output_locs = H_out * W_out
    
    print *, "🚀 Fused im2col+GEMM CPU convolution (Mini v2 - correct column-major)"
    print '(A,I0,A)', "   Tile size: ", TILE_SIZE, " output locations"
    
    call cpu_time(start_time)
    
    ! Initialize output
    output = 0.0
    
    ! Process output locations in tiles
    num_tiles = (total_output_locs + TILE_SIZE - 1) / TILE_SIZE
    
    !$omp parallel private(tile_idx, tile_start, tile_end, tile_cols, &
    !$omp&                h_out_idx, w_out_idx, out_idx, out_loc, &
    !$omp&                n_idx, c_idx, kh_idx, kw_idx, &
    !$omp&                h_in, w_in, in_idx, k_idx, &
    !$omp&                linear_idx, output_offset, &
    !$omp&                row_idx, col_idx, &
    !$omp&                im2col_tile, output_tile)
    
    ! Allocate thread-local workspace
    allocate(im2col_tile(input_rows * TILE_SIZE))
    allocate(output_tile(K * TILE_SIZE))
    
    !$omp do
    do tile_idx = 1, num_tiles
      tile_start = (tile_idx - 1) * TILE_SIZE + 1
      tile_end = min(tile_start + TILE_SIZE - 1, total_output_locs)
      tile_cols = tile_end - tile_start + 1
      
      ! Initialize tiles
      im2col_tile = 0.0
      output_tile = 0.0
      
      ! IM2COL PACKING CONTRACT (Mini's recommendation)
      ! ==========================================
      ! Pack column-by-column (Fortran-friendly)
      ! B(i,j) layout: i = input_row, j = output_position
      ! Memory increases fastest in i (row index)
      
      ! Fill im2col tile - COLUMN BY COLUMN as Mini advised!
      do col_idx = 1, tile_cols  ! Outer loop: columns (output positions)
        linear_idx = tile_start + col_idx - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        row_idx = 0
        do n_idx = 1, N           ! Inner loops: rows (input dimensions)
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                row_idx = row_idx + 1
                
                h_in = (h_out_idx-1)*stride + kh_idx - pad
                w_in = (w_out_idx-1)*stride + kw_idx - pad
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  in_idx = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  ! Column-major storage: element at (row_idx, col_idx)
                  im2col_tile(row_idx + (col_idx-1)*input_rows) = input(in_idx)
                else
                  im2col_tile(row_idx + (col_idx-1)*input_rows) = 0.0
                end if
              end do
            end do
          end do
        end do
      end do
      
      ! Debug check for first tile (Mini's stride probe idea)
      if (tile_idx == 1 .and. tile_cols >= 2) then
        print *, "   Debug: First 2x2 block of im2col_tile:"
        print '(A,2F10.4)', "   Row 1: ", im2col_tile(1), im2col_tile(1 + input_rows)
        print '(A,2F10.4)', "   Row 2: ", im2col_tile(2), im2col_tile(2 + input_rows)
      end if
      
      ! GEMM with proper leading dimensions
      ! C = alpha * A * B + beta * C
      ! output_tile(K, tile_cols) = weights(K, input_rows) * im2col_tile(input_rows, tile_cols)
      call gemm_simd_avx512_v2(weights, im2col_tile(1:input_rows*tile_cols), &
                              output_tile(1:K*tile_cols), &
                              K, tile_cols, input_rows, &
                              1.0, 0.0, &
                              K, input_rows, K)  ! lda=K, ldb=input_rows, ldc=K
      
      ! Write results back
      do col_idx = 1, tile_cols
        linear_idx = tile_start + col_idx - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        do k_idx = 1, K
          output_offset = (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
          ! Read from column-major output_tile
          output(output_offset) = output_tile(k_idx + (col_idx-1)*K)
        end do
      end do
    end do
    !$omp end do
    
    deallocate(im2col_tile, output_tile)
    !$omp end parallel
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_mini_v2 = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_mini_v2, real64) * 1.0e6_real64)
    
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_mini_v2, " ms, ", gflops, " GFLOPS"
    print *, "   ✅ Column-major packing (Fortran-friendly)"
    print *, "   ✅ Memory increases fastest in row index"
    
  end function conv2d_fused_mini_v2

end module cpu_conv2d_fused_mini_v2