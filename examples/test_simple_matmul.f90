program test_simple_matmul
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_memory
  implicit none
  
  type(memory_handle) :: a_mem, b_mem, c_mem
  real(real32), pointer :: a(:,:), b(:,:), c(:,:)
  integer :: i, j, k
  integer :: n = 4
  real(real64) :: start_time, end_time
  
  print *, "🧪 Simple Matrix Multiplication Test"
  print *, "==================================="
  
  ! Allocate memory
  a_mem = create_memory(int(n * n * 4, int64))
  b_mem = create_memory(int(n * n * 4, int64))
  c_mem = create_memory(int(n * n * 4, int64))
  
  ! Get pointers
  call c_f_pointer(a_mem%ptr, a, [n, n])
  call c_f_pointer(b_mem%ptr, b, [n, n])
  call c_f_pointer(c_mem%ptr, c, [n, n])
  
  ! Initialize small matrices
  a = reshape([1.0, 2.0, 3.0, 4.0, &
               5.0, 6.0, 7.0, 8.0, &
               9.0, 10.0, 11.0, 12.0, &
               13.0, 14.0, 15.0, 16.0], [n, n])
               
  b = reshape([1.0, 0.0, 0.0, 0.0, &
               0.0, 1.0, 0.0, 0.0, &
               0.0, 0.0, 1.0, 0.0, &
               0.0, 0.0, 0.0, 1.0], [n, n])  ! Identity matrix
  
  print *, "Matrix A:"
  do i = 1, n
    print '(4F8.2)', a(i,:)
  end do
  
  print *, ""
  print *, "Matrix B (identity):"
  do i = 1, n
    print '(4F8.2)', b(i,:)
  end do
  
  ! Direct Fortran matrix multiplication
  call cpu_time(start_time)
  
  ! Manual multiplication
  do j = 1, n
    do i = 1, n
      c(i,j) = 0.0
      do k = 1, n
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do
  
  call cpu_time(end_time)
  
  print *, ""
  print *, "Result C = A × B:"
  do i = 1, n
    print '(4F8.2)', c(i,:)
  end do
  
  print *, ""
  print '(A,F10.6,A)', "Time: ", (end_time - start_time) * 1000.0, " ms"
  
  ! Verify (A × I = A)
  if (all(abs(c - a) < 1.0e-6)) then
    print *, "✓ Result is correct (A × I = A)"
  else
    print *, "✗ Result is incorrect!"
  end if
  
  ! Now test with Fortran intrinsic
  c = matmul(a, b)
  print *, ""
  print *, "Using Fortran intrinsic matmul:"
  do i = 1, n
    print '(4F8.2)', c(i,:)
  end do
  
  ! Cleanup
  call destroy_memory(a_mem)
  call destroy_memory(b_mem)
  call destroy_memory(c_mem)
  
  print *, ""
  print *, "🎉 Test complete!"

end program test_simple_matmul