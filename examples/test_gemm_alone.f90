program test_gemm_alone
  use iso_fortran_env
  use gemm_simd_optimized, only: gemm_simd_avx512
  use gemm_reference, only: gemm_ref
  implicit none
  
  real(real32), allocatable :: A(:,:), B(:,:), C1(:,:), C2(:,:)
  real(real32), allocatable :: A_flat(:), B_flat(:), C1_flat(:), C2_flat(:)
  integer :: m, n, k
  integer :: i, j, kk
  real(real32) :: max_diff
  
  print *, "=== TESTING GEMM ALONE ==="
  print *, ""
  
  ! Small test
  m = 4
  n = 3
  k = 2
  
  allocate(A(m, k), B(k, n), C1(m, n), C2(m, n))
  allocate(A_flat(m*k), B_flat(k*n), C1_flat(m*n), C2_flat(m*n))
  
  ! Initialize matrices
  A = reshape([1.0, 2.0, 3.0, 4.0, &
               5.0, 6.0, 7.0, 8.0], [m, k])
  
  B = reshape([1.0, 2.0, &
               3.0, 4.0, &
               5.0, 6.0], [k, n])
  
  ! Convert to flat arrays (column-major)
  A_flat = reshape(A, [m*k])
  B_flat = reshape(B, [k*n])
  
  print *, ""
  print *, "A_flat:", A_flat
  print *, "B_flat:", B_flat
  
  print *, "Matrix A (", m, "x", k, "):"
  do i = 1, m
    print '(2F8.2)', A(i,:)
  end do
  
  print *, ""
  print *, "Matrix B (", k, "x", n, "):"
  do i = 1, k
    print '(3F8.2)', B(i,:)
  end do
  
  ! Manual calculation for reference
  C1 = 0.0
  do j = 1, n
    do i = 1, m
      do kk = 1, k
        C1(i,j) = C1(i,j) + A(i,kk) * B(kk,j)
      end do
    end do
  end do
  
  print *, ""
  print *, "Expected result C = A*B:"
  do i = 1, m
    print '(3F8.2)', C1(i,:)
  end do
  
  ! Test reference GEMM first
  C2_flat = 0.0
  call gemm_ref(A_flat, B_flat, C2_flat, m, n, k, 1.0, 0.0)
  C2 = reshape(C2_flat, [m, n])
  
  print *, ""
  print *, "Reference GEMM result:"
  do i = 1, m
    print '(3F8.2)', C2(i,:)
  end do
  
  ! Test SIMD GEMM
  C2_flat = 0.0
  call gemm_simd_avx512(A_flat, B_flat, C2_flat, m, n, k, 1.0, 0.0)
  C2 = reshape(C2_flat, [m, n])
  
  print *, ""
  print *, "SIMD GEMM result:"
  do i = 1, m
    print '(3F8.2)', C2(i,:)
  end do
  
  max_diff = maxval(abs(C1 - C2))
  print *, ""
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-5) then
    print *, "✅ GEMM works correctly!"
  else
    print *, "❌ GEMM has issues!"
  end if
  
  deallocate(A, B, C1, C2, A_flat, B_flat, C1_flat, C2_flat)
  
end program test_gemm_alone