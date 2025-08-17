program test_debug_im2col
  use iso_fortran_env
  implicit none
  
  integer :: N, C, H, W, kernel_size, stride, pad, H_out, W_out
  integer :: n_idx, c_idx, h_out_idx, w_out_idx, kh_idx, kw_idx
  integer :: h_in, w_in, in_idx, out_idx, col_idx
  
  ! Tiny test case
  N = 1
  C = 2
  H = 4
  W = 4
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "=== IM2COL INDEXING DEBUG ==="
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0)', "Output patches: ", H_out, "x", W_out
  print *, ""
  
  ! Show how unfused im2col indexes
  print *, "UNFUSED im2col indexing (from universal_memory_optimization.f90):"
  print *, "Processing first output location (h_out=1, w_out=1):"
  
  h_out_idx = 1
  w_out_idx = 1
  out_idx = (h_out_idx-1)*W_out + w_out_idx
  
  do n_idx = 1, N
    do c_idx = 1, C  
      do kh_idx = 1, kernel_size
        do kw_idx = 1, kernel_size
          h_in = (h_out_idx-1)*stride + kh_idx - pad
          w_in = (w_out_idx-1)*stride + kw_idx - pad
          
          ! Column-major layout for optimal GEMM performance
          col_idx = ((((kh_idx-1)*kernel_size + (kw_idx-1))*C + (c_idx-1))*N + (n_idx-1))*H_out*W_out + out_idx
          
          print '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)', &
            "  n=", n_idx, " c=", c_idx, " kh=", kh_idx, " kw=", kw_idx, &
            " -> h_in=", h_in, " w_in=", w_in, " col_idx=", col_idx
        end do
      end do
    end do
  end do
  
  print *, ""
  print *, "For GEMM, im2col matrix has shape:"
  print '(A,I0,A,I0)', "  rows = C*K*K = ", C*kernel_size*kernel_size, " (input channels)"
  print '(A,I0,A,I0)', "  cols = H_out*W_out = ", H_out*W_out, " (output locations)"
  
  print *, ""
  print *, "FUSED approach should process same data, but in tiles"
  print *, "For tile containing first output location:"
  print *, "  - Extract patch data into tile buffer (keeping hot in cache)"
  print *, "  - GEMM immediately while hot"
  print *, "  - Move to next tile"
  
end program test_debug_im2col