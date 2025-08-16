program test_amd_conv_simple
  use iso_c_binding
  use sparkle_types
  use sparkle_glsl_compute
  use sparkle_glsl_generator
  implicit none
  
  type(glsl_kernel) :: kernel
  real :: start_time, end_time
  integer :: i, j
  
  ! Test sizes - small for verification
  integer :: M = 16    ! Output channels
  integer :: N = 900   ! Output spatial size (30x30)
  integer :: K = 27    ! Input patch size (3x3x3)
  
  real, allocatable :: A(:,:), B(:,:), C(:,:), C_ref(:,:)
  real :: alpha = 1.0, beta = 0.0
  real :: error
  
  print *, "=== Testing Convolution as GEMM on AMD GPU ==="
  print *, ""
  print *, "Convolution parameters:"
  print *, "  Output channels:", M
  print *, "  Output locations:", N
  print *, "  Input patch size:", K
  print *, "  Equivalent to 3x3 conv on 32x32 image with 3 input channels"
  print *, ""
  
  ! Initialize GLSL compute
  call init_glsl_compute()
  
  ! Create kernel
  kernel = create_glsl_kernel("conv_gemm", M, N, K)
  if (.not. kernel%is_initialized) then
    print *, "Failed to create GLSL kernel"
    stop 1
  end if
  
  print *, "✅ Created GLSL compute kernel"
  print *, "  Work groups:", kernel%work_groups_x, "x", kernel%work_groups_y
  print *, ""
  
  ! Allocate matrices
  allocate(A(M, K))      ! Weight matrix (transposed for col-major)
  allocate(B(K, N))      ! Im2col matrix
  allocate(C(M, N))      ! Output
  allocate(C_ref(M, N))  ! Reference
  
  ! Initialize with test data
  ! Weights: simple patterns for each output channel
  do i = 1, M
    do j = 1, K
      A(i, j) = real(i) * 0.1 + real(j) * 0.01
    end do
  end do
  
  ! Input patches: simulate im2col output
  do i = 1, K
    do j = 1, N
      B(i, j) = sin(real(i + j) * 0.1)
    end do
  end do
  
  C = 0.0
  
  ! Upload data
  call upload_matrix(kernel%buffers(1), A)
  call upload_matrix(kernel%buffers(2), B)
  call upload_matrix(kernel%buffers(3), C)
  
  print *, "Executing convolution kernel on GPU..."
  
  ! Execute kernel
  call cpu_time(start_time)
  call execute_glsl_kernel(kernel)
  call cpu_time(end_time)
  
  print *, "✅ Kernel executed in", (end_time - start_time) * 1000, "ms"
  
  ! Download result
  call download_matrix(kernel%buffers(3), C)
  
  ! Compute reference on CPU
  print *, ""
  print *, "Computing reference on CPU..."
  call cpu_time(start_time)
  call gemm_reference(M, N, K, alpha, A, B, beta, C_ref)
  call cpu_time(end_time)
  
  print *, "CPU reference computed in", (end_time - start_time) * 1000, "ms"
  
  ! Verify results
  print *, ""
  print *, "Verification (first few outputs):"
  error = 0.0
  do i = 1, min(5, M)
    do j = 1, min(5, N)
      if (j <= 3) then
        print '(A,I2,A,I3,A,F8.4,A,F8.4)', &
          "  C[", i, ",", j, "] = ", C(i,j), " (ref: ", C_ref(i,j), ")"
      end if
      error = error + abs(C(i,j) - C_ref(i,j))
    end do
  end do
  
  error = error / (M * N)
  print *, ""
  print *, "Average absolute error:", error
  
  if (error < 0.001) then
    print *, ""
    print *, "✅ SUCCESS! Convolution as GEMM working on AMD GPU!"
    
    ! Run a larger test for performance
    print *, ""
    print *, "Running larger convolution (64 output channels, 224x224 image):"
    
    ! Reconfigure for larger size
    M = 64
    N = 50176  ! 224x224 
    K = 27     ! 3x3x3
    
    deallocate(A, B, C, C_ref)
    allocate(A(M, K))
    allocate(B(K, N))
    allocate(C(M, N))
    
    ! Reinitialize
    do i = 1, M
      do j = 1, K
        A(i, j) = real(i + j) * 0.001
      end do
    end do
    
    do i = 1, K
      do j = 1, N
        B(i, j) = real(mod(i * j, 100)) * 0.01
      end do
    end do
    
    C = 0.0
    
    ! Create new kernel for larger size
    call destroy_glsl_kernel(kernel)
    kernel = create_glsl_kernel("conv_gemm_large", M, N, K)
    
    ! Upload and execute
    call upload_matrix(kernel%buffers(1), A)
    call upload_matrix(kernel%buffers(2), B)
    call upload_matrix(kernel%buffers(3), C)
    
    call cpu_time(start_time)
    call execute_glsl_kernel(kernel)
    call cpu_time(end_time)
    
    print *, "  GPU time:", (end_time - start_time) * 1000, "ms"
    print *, "  GFLOPS:", 2.0 * M * N * K / ((end_time - start_time) * 1e9)
    
  else
    print *, "❌ Results don't match!"
  end if
  
  ! Cleanup
  call destroy_glsl_kernel(kernel)
  call cleanup_glsl_compute()
  
  deallocate(A, B, C)
  if (allocated(C_ref)) deallocate(C_ref)
  
contains

  subroutine gemm_reference(M, N, K, alpha, A, B, beta, C)
    integer, intent(in) :: M, N, K
    real, intent(in) :: alpha, beta
    real, intent(in) :: A(M, K), B(K, N)
    real, intent(inout) :: C(M, N)
    
    integer :: i, j, k
    real :: sum
    
    do j = 1, N
      do i = 1, M
        sum = 0.0
        do k = 1, K
          sum = sum + A(i, k) * B(k, j)
        end do
        C(i, j) = alpha * sum + beta * C(i, j)
      end do
    end do
  end subroutine gemm_reference
  
end program test_amd_conv_simple