! SIMD-optimized im2col transformation
! Target: Reduce im2col overhead from 94% to <50%
module im2col_simd_optimized
  use iso_fortran_env, only: real32, int32, int64
  implicit none
  
  private
  public :: im2col_simd_avx512, im2col_cache_blocked
  
contains

  ! AVX-512 optimized im2col with vectorized memory access
  subroutine im2col_simd_avx512(input, im2col_buffer, &
                                N, C, H, W, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(*)
    real(real32), intent(out) :: im2col_buffer(*)
    integer, intent(in) :: N, C, H, W, kernel_size, stride, pad, H_out, W_out
    
    integer :: ho, wo, ch, kh, kw
    integer :: h_in, w_in, in_idx, out_idx
    integer :: col_idx
    integer :: h_start, w_start
    integer :: vector_size = 16  ! AVX-512 can process 16 floats
    
    ! For each output location
    col_idx = 0
    
    !$omp parallel do collapse(2) private(h_start, w_start, ch, kh, kw, &
    !$omp                                  h_in, w_in, in_idx, out_idx, col_idx)
    do ho = 1, H_out
      do wo = 1, W_out
        col_idx = ((ho-1) * W_out + (wo-1)) * C * kernel_size * kernel_size
        
        h_start = (ho - 1) * stride - pad + 1
        w_start = (wo - 1) * stride - pad + 1
        
        ! For each input channel
        do ch = 1, C
          ! For each kernel position
          do kh = 1, kernel_size
            h_in = h_start + kh - 1
            
            ! Check if entire row is valid
            if (h_in >= 1 .and. h_in <= H) then
              ! Vectorized copy of kernel row
              !$omp simd
              do kw = 1, kernel_size
                w_in = w_start + kw - 1
                col_idx = col_idx + 1
                
                if (w_in >= 1 .and. w_in <= W) then
                  in_idx = ((N-1)*C + (ch-1))*H*W + (h_in-1)*W + w_in
                  im2col_buffer(col_idx) = input(in_idx)
                else
                  im2col_buffer(col_idx) = 0.0
                end if
              end do
            else
              ! Entire row is out of bounds
              !$omp simd
              do kw = 1, kernel_size
                col_idx = col_idx + 1
                im2col_buffer(col_idx) = 0.0
              end do
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do
    
  end subroutine im2col_simd_avx512
  
  ! Cache-blocked im2col for better memory access patterns
  subroutine im2col_cache_blocked(input, im2col_buffer, &
                                  N, C, H, W, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(*)
    real(real32), intent(out) :: im2col_buffer(*)
    integer, intent(in) :: N, C, H, W, kernel_size, stride, pad, H_out, W_out
    
    integer, parameter :: TILE_H = 8   ! Process 8 output rows at a time
    integer, parameter :: TILE_W = 8   ! Process 8 output cols at a time
    integer, parameter :: CHAN_BLOCK = 16  ! Process 16 channels at a time
    
    integer :: h_tile, w_tile, c_block
    integer :: h_start, h_end, w_start, w_end, c_start, c_end
    integer :: ho_idx, wo_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx, col_idx
    real(real32) :: val
    
    ! Process in tiles for better cache usage
    !$omp parallel do collapse(3) private(h_start, h_end, w_start, w_end, &
    !$omp                                 c_start, c_end, ho_idx, wo_idx, c_idx, &
    !$omp                                 kh_idx, kw_idx, h_in, w_in, in_idx, col_idx, val)
    do h_tile = 1, H_out, TILE_H
      do w_tile = 1, W_out, TILE_W
        do c_block = 1, C, CHAN_BLOCK
          h_start = h_tile
          h_end = min(h_tile + TILE_H - 1, H_out)
          w_start = w_tile
          w_end = min(w_tile + TILE_W - 1, W_out)
          c_start = c_block
          c_end = min(c_block + CHAN_BLOCK - 1, C)
          
          ! Process tile
          do ho_idx = h_start, h_end
            do wo_idx = w_start, w_end
              do c_idx = c_start, c_end
                do kh_idx = 1, kernel_size
                  h_in = (ho_idx - 1) * stride + kh_idx - pad
                  
                  ! Vectorize innermost loop
                  !$omp simd private(w_in, in_idx, val)
                  do kw_idx = 1, kernel_size
                    w_in = (wo_idx - 1) * stride + kw_idx - pad
                    
                    col_idx = ((ho_idx-1)*W_out + (wo_idx-1))*C*kernel_size*kernel_size + &
                             ((c_idx-1)*kernel_size*kernel_size + (kh_idx-1)*kernel_size + kw_idx)
                    
                    if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                      in_idx = ((N-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                      val = input(in_idx)
                    else
                      val = 0.0
                    end if
                    
                    im2col_buffer(col_idx) = val
                  end do
                  !$omp end simd
                end do
              end do
            end do
          end do
        end do
      end do
    end do
    !$omp end parallel do
    
  end subroutine im2col_cache_blocked
  
end module im2col_simd_optimized