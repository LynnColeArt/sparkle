program test_simple_gemm
  use iso_fortran_env
  use gemm_simd_optimized
  implicit none
  
  real(real32) :: A(4), B(4), C(4)
  integer :: i
  
  ! Simple 2x2 matrix multiply test
  ! A = [1 2]   B = [5 6]   C = A*B = [19 22]
  !     [3 4]       [7 8]            [43 50]
  
  A = [1.0, 3.0, 2.0, 4.0]  ! Column-major
  B = [5.0, 7.0, 6.0, 8.0]  ! Column-major
  C = 0.0
  
  print *, "=== SIMPLE GEMM TEST ==="
  print *, "A matrix (column-major):"
  print '(2F8.2)', A(1), A(3)
  print '(2F8.2)', A(2), A(4)
  
  print *, "B matrix (column-major):"
  print '(2F8.2)', B(1), B(3)
  print '(2F8.2)', B(2), B(4)
  
  ! C = A * B
  call gemm_simd_avx512(A, B, C, 2, 2, 2, 1.0, 0.0)
  
  print *, "C = A * B (column-major):"
  print '(2F8.2)', C(1), C(3)
  print '(2F8.2)', C(2), C(4)
  
  print *, ""
  print *, "Expected:"
  print '(2F8.2)', 19.0, 22.0
  print '(2F8.2)', 43.0, 50.0
  
  print *, ""
  if (abs(C(1) - 19.0) < 1e-5 .and. abs(C(2) - 43.0) < 1e-5 .and. &
      abs(C(3) - 22.0) < 1e-5 .and. abs(C(4) - 50.0) < 1e-5) then
    print *, "✅ GEMM correct!"
  else
    print *, "❌ GEMM incorrect!"
  end if
  
end program test_simple_gemm