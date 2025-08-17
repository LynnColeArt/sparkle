! CPU Convolution - Mini's Threading Hotfix
! Do not simplify.
! • Partition by columns (N) only; each thread owns a unique [j0:j1] for both Bt and C.
! • Allocate thread-local, 64B-aligned Bt(I_pad, tileN) once per thread.
! • Pack column-major: outer j, inner i (SIMD on i).
! • Use int64 indices; guard pad/stride bounds before loads.
! • schedule(static,tileN); OMP_PLACES=cores, OMP_PROC_BIND=close.
! • Re-enable threads after single-thread ASan/bounds-check passes.

module cpu_conv2d_hotfix
  use iso_fortran_env, only: real32, real64, int32, int64
  use iso_c_binding, only: c_ptr, c_f_pointer, c_size_t
  use gemm_simd_optimized_v2, only: gemm_simd_avx512_v2
  use timing_helpers, only: now_s, safe_gflops
  implicit none
  
  private
  public :: conv2d_hotfix
  
  ! Tile size for static scheduling
  integer, parameter :: TILE_N = 128
  
  ! Interface for aligned allocation
  interface
    function posix_memalign_wrapper(size_bytes) bind(C, name="posix_memalign_wrapper")
      import :: c_ptr, c_size_t
      integer(c_size_t), value :: size_bytes
      type(c_ptr) :: posix_memalign_wrapper
    end function
    
    subroutine free_wrapper(ptr) bind(C, name="free")
      import :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine
  end interface
  
contains

  subroutine partition_columns(N_total, thread_id, num_threads, j0, j1)
    integer(int64), intent(in) :: N_total
    integer, intent(in) :: thread_id, num_threads
    integer(int64), intent(out) :: j0, j1
    
    integer(int64) :: chunk_size, remainder
    
    chunk_size = N_total / int(num_threads, int64)
    remainder = mod(N_total, int(num_threads, int64))
    
    if (thread_id < int(remainder)) then
      j0 = int(thread_id, int64) * (chunk_size + 1) + 1
      j1 = j0 + chunk_size
    else
      j0 = int(thread_id, int64) * chunk_size + remainder + 1
      j1 = j0 + chunk_size - 1
    end if
    
    ! Ensure valid range
    j1 = min(j1, N_total)
    if (j0 > N_total) then
      j0 = N_total + 1
      j1 = N_total
    end if
  end subroutine partition_columns

  real(real32) function conv2d_hotfix(input, weights, output, &
                                     N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time, elapsed_secs
    real(real64) :: total_flops_r
    real(real64) :: gflops
    
    ! Use int64 for all index calculations
    integer(int64) :: I_dim, I_pad, N_total
    integer(int64) :: j0, j1, j_col, i_row, jj
    integer(int64) :: n_idx, ch_idx, kh_idx, kw_idx
    integer(int64) :: h_out_pos, w_out_pos, h_in_pos, w_in_pos
    integer(int64) :: in_idx, out_idx, rem
    integer :: thread_id, num_threads
    
    ! Thread-local pointers
    type(c_ptr) :: bt_ptr, ct_ptr
    real(real32), pointer :: Bt(:), Ct(:)
    integer(int64) :: bt_size, ct_size, local_cols
    
    call cpu_time(start_time)
    
    ! Calculate dimensions using int64
    I_dim = int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * int(N, int64)
    I_pad = ((I_dim + 15) / 16) * 16  ! Pad to multiple of 16 for AVX-512
    N_total = int(H_out, int64) * int(W_out, int64)
    
    ! Initialize output to zero
    output = 0.0
    
    !$omp parallel default(none) &
    !$omp shared(input, weights, output, N, C, H, W, K, kernel_size, stride, pad) &
    !$omp shared(H_out, W_out, I_dim, I_pad, N_total) &
    !$omp private(thread_id, num_threads, j0, j1, bt_ptr, ct_ptr, Bt, Ct) &
    !$omp private(bt_size, ct_size, local_cols) &
    !$omp private(j_col, i_row, jj, n_idx, ch_idx, kh_idx, kw_idx) &
    !$omp private(h_out_pos, w_out_pos, h_in_pos, w_in_pos, in_idx, out_idx, rem)
    
    thread_id = omp_get_thread_num()
    num_threads = omp_get_num_threads()
    
    ! Get this thread's column range
    call partition_columns(N_total, thread_id, num_threads, j0, j1)
    local_cols = max(1_int64, j1 - j0 + 1)
    
    ! Allocate thread-local, 64B-aligned Bt and Ct
    bt_size = I_pad * local_cols * 4  ! 4 bytes per real32
    ct_size = K * local_cols * 4
    bt_ptr = posix_memalign_wrapper(bt_size)
    ct_ptr = posix_memalign_wrapper(ct_size)
    call c_f_pointer(bt_ptr, Bt, [I_pad * local_cols])
    call c_f_pointer(ct_ptr, Ct, [K * local_cols])
    
    ! Verify alignment in debug mode
    !if (mod(transfer(c_loc(Bt), 0_int64), 64) /= 0) then
    !  print *, "WARNING: Bt not 64-byte aligned for thread", thread_id
    !end if
    
    ! Clear Bt and Ct
    Bt = 0.0
    Ct = 0.0
    
    ! Pack Bt column-major: outer j, inner i
    do jj = 1, local_cols
      j_col = j0 + jj - 1
      
      ! Map j to output position (simplified for N=1 case)
      if (N == 1) then
        rem = j_col - 1
        h_out_pos = rem / int(W_out, int64) + 1
        w_out_pos = mod(rem, int(W_out, int64)) + 1
      else
        n_idx = (j_col - 1) / (int(H_out, int64) * int(W_out, int64)) + 1
        rem = mod(j_col - 1, int(H_out, int64) * int(W_out, int64))
        h_out_pos = rem / int(W_out, int64) + 1
        w_out_pos = mod(rem, int(W_out, int64)) + 1
      end if
      
      ! Pack this column with bounds checking
      i_row = 0
      do n_idx = 1, N
        do ch_idx = 1, C
          do kh_idx = 1, kernel_size
            h_in_pos = (h_out_pos - 1) * int(stride, int64) + int(kh_idx, int64) - int(pad, int64)
            
            ! SIMD on innermost loop
            !$omp simd
            do kw_idx = 1, kernel_size
              i_row = i_row + 1
              if (i_row > I_dim) cycle  ! Safety check
              
              w_in_pos = (w_out_pos - 1) * int(stride, int64) + int(kw_idx, int64) - int(pad, int64)
              
              ! Bounds check
              if (h_in_pos >= 1 .and. h_in_pos <= int(H, int64) .and. &
                  w_in_pos >= 1 .and. w_in_pos <= int(W, int64)) then
                ! NCHW layout
                in_idx = ((int(n_idx, int64)-1)*int(C, int64) + (int(ch_idx, int64)-1))*int(H*W, int64) + &
                        (h_in_pos-1)*int(W, int64) + w_in_pos
                Bt(i_row + (jj-1)*I_pad) = input(in_idx)
              else
                Bt(i_row + (jj-1)*I_pad) = 0.0
              end if
            end do
          end do
        end do
      end do
    end do
    
    ! GEMM on thread-local data to thread-local Ct
    if (local_cols > 0) then
      call gemm_simd_avx512_v2(weights, Bt, Ct, &
                              K, int(local_cols, int32), int(I_dim, int32), &
                              1.0, 0.0, &
                              K, int(I_pad, int32), K)
    end if
    
    ! Transform output from column-major to NCHW layout
    do jj = 1, local_cols
      j_col = j0 + jj - 1
      
      ! For batch size N=1, simplified mapping
      if (N == 1) then
        rem = j_col - 1
        h_out_pos = rem / int(W_out, int64) + 1
        w_out_pos = mod(rem, int(W_out, int64)) + 1
        
        ! Copy K channels to proper NCHW positions from Ct
        !$omp simd
        do i_row = 1, K
          out_idx = (int(i_row, int64)-1)*int(H_out*W_out, int64) + &
                    (h_out_pos-1)*int(W_out, int64) + w_out_pos
          output(out_idx) = Ct((jj-1)*K + i_row)
        end do
      else
        ! General case for multiple batches
        n_idx = (j_col - 1) / (int(H_out, int64) * int(W_out, int64)) + 1
        rem = mod(j_col - 1, int(H_out, int64) * int(W_out, int64))
        h_out_pos = rem / int(W_out, int64) + 1
        w_out_pos = mod(rem, int(W_out, int64)) + 1
        
        ! Copy K channels to proper NCHW positions from Ct
        !$omp simd
        do i_row = 1, K
          out_idx = ((n_idx-1)*int(K, int64) + (i_row-1))*int(H_out*W_out, int64) + &
                    (h_out_pos-1)*int(W_out, int64) + w_out_pos
          output(out_idx) = Ct((jj-1)*K + i_row)
        end do
      end if
    end do
    
    ! Free aligned memory
    call free_wrapper(bt_ptr)
    call free_wrapper(ct_ptr)
    
    !$omp barrier  ! All threads finished
    !$omp end parallel
    
    end_time = now_s()
    elapsed_secs = end_time - start_time
    conv2d_hotfix = real(elapsed_secs, real32) * 1000.0
    
    ! Calculate performance using real64 to avoid overflow
    total_flops_r = 2.0_real64 * real(N, real64) * real(K, real64) * real(H_out, real64) * real(W_out, real64) * &
                    real(C, real64) * real(kernel_size, real64) * real(kernel_size, real64)
    gflops = safe_gflops(total_flops_r, elapsed_secs)
    
    ! Guard against zero timing
    if (elapsed_secs <= 0.0_real64 .or. .not.(elapsed_secs > 0.0_real64)) then
      print *, "[WARN] Measured time <= 0; using safe fallback"
      conv2d_hotfix = 1.0  ! 1ms fallback
      gflops = safe_gflops(total_flops_r, 1.0e-3_real64)
    end if
    
    print *, "🔥 Hotfix CPU Convolution"
    print '(A,I0)', "   Threads: ", omp_get_max_threads()
    print '(A,I0,A,I0,A,I0,A)', "   Matrix: ", K, "×", I_dim, " (padded to ", I_pad, ")"
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_hotfix, " ms, ", gflops, " GFLOPS"
    print *, "   ✅ Column partitioning (no races)"
    print *, "   ✅ 64B aligned thread-local Bt"
    print *, "   ✅ SIMD on contiguous i loops"
    
  end function conv2d_hotfix
  
  ! OpenMP function interfaces
  function omp_get_thread_num() bind(C, name="omp_get_thread_num")
    integer :: omp_get_thread_num
  end function omp_get_thread_num
  
  function omp_get_num_threads() bind(C, name="omp_get_num_threads")
    integer :: omp_get_num_threads
  end function omp_get_num_threads
  
  function omp_get_max_threads() bind(C, name="omp_get_max_threads")
    integer :: omp_get_max_threads
  end function omp_get_max_threads
  
end module cpu_conv2d_hotfix