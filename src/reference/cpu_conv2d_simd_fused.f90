! Fused im2col+GEMM CPU Convolution with SIMD
! ============================================
!
! This module implements the proper hot-cache convolution
! by fusing im2col and GEMM operations, matching the GPU approach
!
! Key insight: Don't materialize the full im2col buffer!
! Process tiles that fit in L2 cache (~512KB on modern CPUs)

module cpu_conv2d_simd_fused
  use iso_fortran_env, only: real32, real64, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  use omp_lib
  implicit none
  
  private
  public :: conv2d_cpu_simd_fused
  
  ! Tile size for L2 cache (512KB / 4 bytes per float ≈ 128K floats)
  ! We want to fit: tile_m × tile_k input tile + tile_m × tile_n output tile
  integer, parameter :: TILE_M = 256  ! Output channels per tile
  integer, parameter :: TILE_N = 64   ! Spatial positions per tile  
  integer, parameter :: TILE_K = 64   ! Input elements per position
  
contains

  real(real32) function conv2d_cpu_simd_fused(input, weights, output, &
                                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    ! Local tile buffers - these stay hot in cache!
    real(real32), allocatable :: tile_input(:), tile_output(:)
    integer :: im2col_rows, spatial_size
    integer :: tile_k_size, max_tile_n
    
    ! Indices
    integer :: n_idx, ko, ki, so, si
    integer :: h_out_idx, w_out_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, spatial_idx, im2col_idx
    integer :: weight_offset, output_offset
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    call cpu_time(start_time)
    
    ! Calculate dimensions
    im2col_rows = C * kernel_size * kernel_size
    spatial_size = H_out * W_out
    
    ! Allocate tile buffers (these fit in L2 cache)
    tile_k_size = min(TILE_K, im2col_rows)
    max_tile_n = min(TILE_N, spatial_size)
    allocate(tile_input(tile_k_size * max_tile_n))
    allocate(tile_output(TILE_M * max_tile_n))
    
    print *, "🚀 Fused SIMD CPU Convolution (Hot Cache)"
    print '(A,I0,A,I0)', "   Tile size: ", TILE_M, "×", max_tile_n
    print '(A,F6.1,A)', "   Tile memory: ", &
            real(tile_k_size * max_tile_n * 4) / 1024.0, " KB (fits in L2!)"
    
    ! Initialize output
    output = 0.0
    
    ! Process each batch
    do n_idx = 1, N
      
      ! Tile over output channels (K)
      !$OMP PARALLEL DO PRIVATE(ki, so, si, tile_input, tile_output, &
      !$OMP                     h_out_idx, w_out_idx, spatial_idx, &
      !$OMP                     c_idx, kh_idx, kw_idx, h_in, w_in, &
      !$OMP                     im2col_idx, weight_offset, output_offset)
      do ko = 1, K, TILE_M
        ki = min(TILE_M, K - ko + 1)
        
        ! Tile over spatial positions  
        do so = 1, spatial_size, TILE_N
          si = min(TILE_N, spatial_size - so + 1)
          
          ! Clear output tile
          tile_output(1:ki*si) = 0.0
          
          ! Tile over im2col rows (input channels × kernel)
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                
                ! Fill tile_input with im2col data for this channel/kernel position
                ! This is the KEY: we only im2col what fits in cache!
                do spatial_idx = 1, si
                  h_out_idx = ((so + spatial_idx - 2) / W_out) + 1
                  w_out_idx = mod(so + spatial_idx - 2, W_out) + 1
                  
                  h_in = (h_out_idx - 1) * stride + kh_idx - pad
                  w_in = (w_out_idx - 1) * stride + kw_idx - pad
                  
                  if (h_in > 0 .and. h_in <= H .and. w_in > 0 .and. w_in <= W) then
                    tile_input(spatial_idx) = input((n_idx-1)*C*H*W + (c_idx-1)*H*W + (h_in-1)*W + w_in)
                  else
                    tile_input(spatial_idx) = 0.0
                  end if
                end do
                
                ! Get weight offset for this input position
                im2col_idx = (c_idx-1)*kernel_size*kernel_size + (kh_idx-1)*kernel_size + kw_idx
                weight_offset = (ko-1)*im2col_rows + im2col_idx - 1
                
                ! FUSED OPERATION: im2col tile is hot, GEMM it immediately!
                ! This is a rank-1 update: tile_output += weights * tile_input
                call gemm_simd_avx512( &
                  weights(weight_offset+1:weight_offset+ki*im2col_rows:im2col_rows), & ! strided weight access
                  tile_input(1:si), &
                  tile_output(1:ki*si), &
                  ki, si, 1, 1.0, 1.0)  ! Accumulate into tile_output
                
              end do
            end do
          end do
          
          ! Write tile_output back to global output
          do spatial_idx = 1, si
            output_offset = (n_idx-1)*K*spatial_size + (ko-1)*spatial_size + (so + spatial_idx - 2)
            output(output_offset+1:output_offset+ki*spatial_size:spatial_size) = &
              tile_output((spatial_idx-1)*ki+1:spatial_idx*ki)
          end do
          
        end do ! spatial tiles
      end do ! output channel tiles
      !$OMP END PARALLEL DO
      
    end do ! batch
    
    call cpu_time(end_time)
    conv2d_cpu_simd_fused = real((end_time - start_time) * 1000.0, real32)
    
    ! Calculate performance
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    gflops = real(total_flops, real64) / ((end_time - start_time) * 1.0e9_real64)
    
    print '(A,F10.2,A,F8.1,A)', "   Performance: ", conv2d_cpu_simd_fused, " ms, ", gflops, " GFLOPS"
    
    if (gflops > 150.0) then
      print *, "   🚀 TRUE hot cache performance achieved!"
    else if (gflops > 100.0) then  
      print *, "   ✅ Good fused performance"
    else
      print *, "   ⚠️  Performance still needs tuning"
    end if
    
    deallocate(tile_input, tile_output)
    
  end function conv2d_cpu_simd_fused

end module cpu_conv2d_simd_fused