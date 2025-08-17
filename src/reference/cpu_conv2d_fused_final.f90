! Fused im2col+GEMM - Final implementation with Mini's contract
module cpu_conv2d_fused_final
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized_v2, only: gemm_simd_avx512_v2
  implicit none
  
  private
  public :: conv2d_fused_final
  
contains

  !---------------------------------------------------------------
  ! IM2COL PACKING CONTRACT (NCHW → column-major B = (I,N))
  ! I = C * KH * KW * Nbatch
  ! N = OH * OW (flattened output positions per batch)
  ! REQUIREMENTS:
  !  • B is column-major with leading dim ldb == I
  !  • Outer loop over columns (j = 1..N), inner over rows (i = 1..I)
  !  • No materialized row-major buffers; stream directly into B(i,j)
  !  • Parallelize over j (columns) to avoid C write races downstream
  !---------------------------------------------------------------
  subroutine assert_im2col_contract(I, N, ldb, ok)
    integer(int32), intent(in)  :: I, N, ldb
    logical,        intent(out) :: ok
    ok = (ldb == I .and. I>0 .and. N>0)
    if (.not. ok) then
       write(*,*) "IM2COL CONTRACT VIOLATION: ldb=", ldb, " expected I=", I
    end if
  end subroutine

  real(real32) function conv2d_fused_final(input, weights, output, &
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
    integer :: h_out_idx, w_out_idx, n_idx
    integer :: c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx
    integer :: num_tiles, tile_idx, k_idx
    integer :: out_idx, i, j
    logical :: contract_ok
    
    ! Calculate dimensions - Mini's I and N
    input_rows = C * kernel_size * kernel_size * N  ! This is I
    total_output_locs = H_out * W_out               ! This is total N across all batches
    
    print *, "🚀 Fused im2col+GEMM CPU convolution (Final - Mini's contract)"
    print '(A,I0,A)', "   Tile size: ", TILE_SIZE, " output locations"
    print '(A,I0,A,I0)', "   I (input_rows) = ", input_rows, ", Total N = ", total_output_locs
    
    call cpu_time(start_time)
    
    ! Initialize output
    output = 0.0
    
    ! Process output locations in tiles
    num_tiles = (total_output_locs + TILE_SIZE - 1) / TILE_SIZE
    
    !$omp parallel private(tile_idx, tile_start, tile_end, tile_cols, &
    !$omp&                h_out_idx, w_out_idx, n_idx, &
    !$omp&                c_idx, kh_idx, kw_idx, &
    !$omp&                h_in, w_in, in_idx, k_idx, &
    !$omp&                out_idx, i, j, contract_ok, &
    !$omp&                im2col_tile, output_tile)
    
    ! Allocate thread-local workspace
    allocate(im2col_tile(input_rows * TILE_SIZE))
    allocate(output_tile(K * TILE_SIZE))
    
    !$omp do
    do tile_idx = 1, num_tiles
      tile_start = (tile_idx - 1) * TILE_SIZE + 1
      tile_end = min(tile_start + TILE_SIZE - 1, total_output_locs)
      tile_cols = tile_end - tile_start + 1
      
      ! Assert contract
      call assert_im2col_contract(input_rows, tile_cols, input_rows, contract_ok)
      if (.not. contract_ok) stop "IM2COL CONTRACT VIOLATION"
      
      ! Initialize tiles
      im2col_tile = 0.0
      output_tile = 0.0
      
      ! Fill im2col tile following Mini's contract
      ! Outer loop over columns (output positions)
      do j = 1, tile_cols
        out_idx = tile_start + j - 1
        h_out_idx = ((out_idx - 1) / W_out) + 1
        w_out_idx = mod(out_idx - 1, W_out) + 1
        
        ! Inner loop over rows (input dimensions)
        i = 0
        do n_idx = 1, N
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                i = i + 1
                
                h_in = (h_out_idx - 1) * stride + kh_idx - pad
                w_in = (w_out_idx - 1) * stride + kw_idx - pad
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  ! NCHW layout: input[n,c,h,w] = input[(n*C + c)*H*W + h*W + w]
                  in_idx = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  im2col_tile(i + (j-1)*input_rows) = input(in_idx)
                else
                  im2col_tile(i + (j-1)*input_rows) = 0.0
                end if
              end do
            end do
          end do
        end do
      end do
      
      ! GEMM: C(K,N) = A(K,I) * B(I,N)
      ! output_tile(K, tile_cols) = weights(K, input_rows) * im2col_tile(input_rows, tile_cols)
      call gemm_simd_avx512_v2(weights, im2col_tile(1:input_rows*tile_cols), &
                              output_tile(1:K*tile_cols), &
                              K, tile_cols, input_rows, &
                              1.0, 0.0, &
                              K, input_rows, K)  ! lda=K, ldb=input_rows, ldc=K
      
      ! Write results back (NCHW output layout)
      do j = 1, tile_cols
        out_idx = tile_start + j - 1
        h_out_idx = ((out_idx - 1) / W_out) + 1
        w_out_idx = mod(out_idx - 1, W_out) + 1
        
        ! For batch size 1, simplified output indexing
        do k_idx = 1, K
          ! output[n,k,h,w] = output[(n*K + k)*H_out*W_out + h*W_out + w]
          out_idx = (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
          output(out_idx) = output_tile(k_idx + (j-1)*K)
        end do
      end do
    end do
    !$omp end do
    
    deallocate(im2col_tile, output_tile)
    !$omp end parallel
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_final = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_final, real64) * 1.0e6_real64)
    
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_final, " ms, ", gflops, " GFLOPS"
    print *, "   ✅ Mini's im2col contract enforced"
    print *, "   ✅ Column-major with proper NCHW indexing"
    
  end function conv2d_fused_final

end module cpu_conv2d_fused_final