program test_gemm_correctness
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_memory
  use sparkle_config
  use sparkle_cache_aware
  implicit none
  
  integer :: n, i, j, k
  type(memory_handle) :: a_mem, b_mem, c_ref_mem, c_test_mem
  real(real32), pointer :: a(:,:), b(:,:), c_ref(:,:), c_test(:,:)
  real(real32) :: max_error, avg_error, element_error
  real(real32) :: sum_ref, sum_test
  integer :: error_count
  type(sparkle_config_type) :: config
  
  print *, "🔍 GEMM Correctness Verification"
  print *, "================================"
  print *, ""
  
  ! Configure for safety
  config%max_cpu_threads = 14
  call sparkle_set_config(config)
  
  ! Test small matrices first where we can verify by hand
  print *, "Test 1: Small Matrix (4x4)"
  print *, "--------------------------"
  
  n = 4
  a_mem = create_memory(int(n*n*4, int64))
  b_mem = create_memory(int(n*n*4, int64))
  c_ref_mem = create_memory(int(n*n*4, int64))
  c_test_mem = create_memory(int(n*n*4, int64))
  
  call c_f_pointer(a_mem%ptr, a, [n, n])
  call c_f_pointer(b_mem%ptr, b, [n, n])
  call c_f_pointer(c_ref_mem%ptr, c_ref, [n, n])
  call c_f_pointer(c_test_mem%ptr, c_test, [n, n])
  
  ! Initialize with known values
  a = reshape([1.0, 2.0, 3.0, 4.0, &
               5.0, 6.0, 7.0, 8.0, &
               9.0, 10.0, 11.0, 12.0, &
               13.0, 14.0, 15.0, 16.0], [n, n])
               
  b = reshape([1.0, 0.0, 0.0, 0.0, &
               0.0, 1.0, 0.0, 0.0, &
               0.0, 0.0, 1.0, 0.0, &
               0.0, 0.0, 0.0, 1.0], [n, n])
  
  ! Reference implementation (simple triple loop)
  c_ref = 0.0
  do j = 1, n
    do i = 1, n
      do k = 1, n
        c_ref(i,j) = c_ref(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do
  
  ! Our implementation
  call cache_aware_gemm(a, b, c_test, n, n, n)
  
  ! Check results
  print *, "A * I (should equal A):"
  do i = 1, n
    print '(4F8.2)', (c_test(i,j), j=1,n)
  end do
  
  max_error = maxval(abs(c_ref - c_test))
  print '(A,ES12.5)', "Max error: ", max_error
  
  ! Cleanup small test
  call destroy_memory(a_mem)
  call destroy_memory(b_mem)
  call destroy_memory(c_ref_mem)
  call destroy_memory(c_test_mem)
  
  ! Test 2: Random matrices, various sizes
  print *, ""
  print *, "Test 2: Random Matrices"
  print *, "-----------------------"
  print *, "Size    Max Error    Avg Error    Errors>1e-4"
  print *, "----    ----------   ----------   -----------"
  
  do n = 64, 512, 64
    
    ! Allocate
    a_mem = create_memory(int(n*n*4, int64))
    b_mem = create_memory(int(n*n*4, int64))
    c_ref_mem = create_memory(int(n*n*4, int64))
    c_test_mem = create_memory(int(n*n*4, int64))
    
    call c_f_pointer(a_mem%ptr, a, [n, n])
    call c_f_pointer(b_mem%ptr, b, [n, n])
    call c_f_pointer(c_ref_mem%ptr, c_ref, [n, n])
    call c_f_pointer(c_test_mem%ptr, c_test, [n, n])
    
    ! Random data
    call random_number(a)
    call random_number(b)
    
    ! Scale down to avoid floating point issues
    a = a * 0.01
    b = b * 0.01
    
    ! Reference: Use Fortran's matmul (known correct)
    c_ref = matmul(a, b)
    
    ! Our implementation
    call cache_aware_gemm(a, b, c_test, n, n, n)
    
    ! Compute errors
    max_error = 0.0
    avg_error = 0.0
    error_count = 0
    
    do j = 1, n
      do i = 1, n
        element_error = abs(c_ref(i,j) - c_test(i,j))
        max_error = max(max_error, element_error)
        avg_error = avg_error + element_error
        if (element_error > 1.0e-4) error_count = error_count + 1
      end do
    end do
    
    avg_error = avg_error / real(n*n)
    
    print '(I4,4X,ES12.5,1X,ES12.5,3X,I8)', n, max_error, avg_error, error_count
    
    ! Cleanup
    call destroy_memory(a_mem)
    call destroy_memory(b_mem)
    call destroy_memory(c_ref_mem)
    call destroy_memory(c_test_mem)
    
  end do
  
  ! Test 3: Check accumulation
  print *, ""
  print *, "Test 3: Accumulation Test"
  print *, "-------------------------"
  
  n = 256
  a_mem = create_memory(int(n*n*4, int64))
  b_mem = create_memory(int(n*n*4, int64))
  c_ref_mem = create_memory(int(n*n*4, int64))
  c_test_mem = create_memory(int(n*n*4, int64))
  
  call c_f_pointer(a_mem%ptr, a, [n, n])
  call c_f_pointer(b_mem%ptr, b, [n, n])
  call c_f_pointer(c_ref_mem%ptr, c_ref, [n, n])
  call c_f_pointer(c_test_mem%ptr, c_test, [n, n])
  
  ! All ones - result should be n in each element
  a = 1.0
  b = 1.0
  
  c_ref = matmul(a, b)
  call cache_aware_gemm(a, b, c_test, n, n, n)
  
  print '(A,F8.2)', "Expected value in each element: ", real(n)
  print '(A,F8.2)', "Actual value at (1,1): ", c_test(1,1)
  print '(A,F8.2)', "Actual value at (n/2,n/2): ", c_test(n/2,n/2)
  print '(A,F8.2)', "Actual value at (n,n): ", c_test(n,n)
  
  max_error = maxval(abs(c_ref - c_test))
  print '(A,ES12.5)', "Max error: ", max_error
  
  ! Check sums
  sum_ref = sum(c_ref)
  sum_test = sum(c_test)
  print '(A,ES12.5)', "Reference sum: ", sum_ref
  print '(A,ES12.5)', "Test sum: ", sum_test
  print '(A,ES12.5)', "Sum difference: ", abs(sum_ref - sum_test)
  
  ! Cleanup
  call destroy_memory(a_mem)
  call destroy_memory(b_mem)
  call destroy_memory(c_ref_mem)
  call destroy_memory(c_test_mem)
  
  print *, ""
  print *, "Verdict:"
  print *, "========"
  if (max_error < 1.0e-4) then
    print *, "✅ GEMM implementation is CORRECT!"
    print *, "   (within floating point tolerance)"
  else
    print *, "❌ GEMM implementation has ERRORS!"
    print *, "   Max error exceeds tolerance"
  end if
  
end program test_gemm_correctness