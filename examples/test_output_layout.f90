program test_output_layout
  use iso_fortran_env
  implicit none
  
  ! Test how convolution output is laid out
  integer :: N, K, H_out, W_out
  integer :: n_idx, k_idx, h_idx, w_idx, linear_idx
  
  N = 1
  K = 2
  H_out = 3
  W_out = 3
  
  print *, "Output layout for N=", N, ", K=", K, ", H_out=", H_out, ", W_out=", W_out
  print *, "Total elements:", N * K * H_out * W_out
  print *, ""
  
  print *, "Linear index -> (n, k, h, w)"
  linear_idx = 1
  do n_idx = 1, N
    do k_idx = 1, K
      do h_idx = 1, H_out
        do w_idx = 1, W_out
          print '(I3,A,I1,A,I1,A,I1,A,I1,A)', linear_idx, " -> (", &
            n_idx, ",", k_idx, ",", h_idx, ",", w_idx, ")"
          linear_idx = linear_idx + 1
        end do
      end do
    end do
  end do
  
end program test_output_layout