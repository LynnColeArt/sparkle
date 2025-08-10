program test_matrix_kernel
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_memory
  use sparkle_mesh_types
  use sparkle_discovery
  use sparkle_kernels
  use sparkle_execute
  implicit none
  
  type(device_handle), allocatable :: devices(:)
  type(mesh_topology) :: mesh
  type(sparkle_kernel) :: kernel
  type(memory_handle) :: a_mem, b_mem, c_mem
  real(real32), pointer :: a(:,:), b(:,:), c(:,:)
  integer :: num_devices, i, j
  integer(int64) :: m, n, k
  real(real64) :: start_time, end_time
  
  print *, "🧪 Matrix Multiplication with Sparkle"
  print *, "===================================="
  
  ! Discover devices  
  block
    type(mesh_topology) :: temp_mesh
    temp_mesh = scan_devices()
    num_devices = temp_mesh%num_devices
    allocate(devices(num_devices))
    devices = temp_mesh%devices
  end block
  print '(A,I0,A)', "Found ", num_devices, " devices"
  
  ! Create mesh
  mesh%num_devices = num_devices
  allocate(mesh%devices(num_devices))
  mesh%devices = devices
  
  ! Initialize links (fully connected for now)
  block
    integer :: ii, jj, link_count
    link_count = num_devices * (num_devices - 1)
    if (link_count > 0) then
      allocate(mesh%links(link_count))
      link_count = 0
      do ii = 1, num_devices
        do jj = 1, num_devices
          if (ii /= jj) then
            link_count = link_count + 1
            mesh%links(link_count)%src_id = devices(ii)%id
            mesh%links(link_count)%dst_id = devices(jj)%id
            mesh%links(link_count)%bw_gbs = 10.0_real64  ! Assume 10 GB/s
            mesh%links(link_count)%latency_us = 1.0_real64
          end if
        end do
      end do
    end if
  end block
  
  ! Matrix dimensions
  m = 512_int64
  n = 512_int64
  k = 512_int64
  
  print *, ""
  print '(A,I0,A,I0,A,I0,A,I0)', "Multiplying matrices: (", m, "x", k, ") × (", k, "x", n, ")"
  
  ! Allocate memory
  a_mem = create_memory(m * k * 4_int64)  ! A is m×k
  b_mem = create_memory(k * n * 4_int64)  ! B is k×n
  c_mem = create_memory(m * n * 4_int64)  ! C is m×n
  
  ! Get pointers as 2D arrays
  call c_f_pointer(a_mem%ptr, a, [m, k])
  call c_f_pointer(b_mem%ptr, b, [k, n])
  call c_f_pointer(c_mem%ptr, c, [m, n])
  
  ! Initialize matrices
  print *, "Initializing matrices..."
  do j = 1, int(k)
    do i = 1, int(m)
      a(i,j) = real(i + j, real32) * 0.01_real32
    end do
  end do
  
  do j = 1, int(n)
    do i = 1, int(k)
      b(i,j) = real(i - j, real32) * 0.01_real32
    end do
  end do
  
  c = 0.0_real32
  
  ! Build matrix multiplication kernel
  kernel%name = 'matrix_multiply'
  kernel%kernel_type = KERNEL_PURE
  kernel%work_items = m * n  ! Total output elements
  kernel%fortran_proc => matmul_kernel
  
  allocate(kernel%arguments(3))
  
  ! Matrix A
  kernel%arguments(1)%name = 'a'
  kernel%arguments(1)%arg_type = TYPE_REAL32
  kernel%arguments(1)%intent = ARG_IN
  kernel%arguments(1)%rank = 2
  allocate(kernel%arguments(1)%shape(2))
  kernel%arguments(1)%shape = [m, k]
  kernel%arguments(1)%data = a_mem
  
  ! Matrix B
  kernel%arguments(2)%name = 'b'
  kernel%arguments(2)%arg_type = TYPE_REAL32
  kernel%arguments(2)%intent = ARG_IN
  kernel%arguments(2)%rank = 2
  allocate(kernel%arguments(2)%shape(2))
  kernel%arguments(2)%shape = [k, n]
  kernel%arguments(2)%data = b_mem
  
  ! Matrix C
  kernel%arguments(3)%name = 'c'
  kernel%arguments(3)%arg_type = TYPE_REAL32
  kernel%arguments(3)%intent = ARG_OUT
  kernel%arguments(3)%rank = 2
  allocate(kernel%arguments(3)%shape(2))
  kernel%arguments(3)%shape = [m, n]
  kernel%arguments(3)%data = c_mem
  
  ! Execute!
  call cpu_time(start_time)
  call sparkle_run(kernel, mesh)
  call cpu_time(end_time)
  
  print *, ""
  print '(A,F8.3,A)', "Matrix multiplication completed in ", &
        (end_time - start_time) * 1000.0_real64, " ms"
  
  ! Verify a few elements
  print *, ""
  print *, "Verifying results (spot check)..."
  block
    real(real32) :: expected
    logical :: correct
    integer :: row, col
    
    correct = .true.
    
    ! Check element (1,1)
    expected = sum(a(1,:) * b(:,1))
    if (abs(c(1,1) - expected) > 1.0e-4) then
      print '(A,F10.6,A,F10.6)', "ERROR at (1,1): expected ", expected, " got ", c(1,1)
      correct = .false.
    end if
    
    ! Check element (m/2, n/2)
    row = int(m/2)
    col = int(n/2)
    expected = sum(a(row,:) * b(:,col))
    if (abs(c(row,col) - expected) > 1.0e-4) then
      print '(A,I0,A,I0,A,F10.6,A,F10.6)', &
        "ERROR at (", row, ",", col, "): expected ", expected, " got ", c(row,col)
      correct = .false.
    end if
    
    if (correct) then
      print *, "✓ Results are correct!"
      print '(A,ES12.5)', "Frobenius norm of C: ", sqrt(sum(c**2))
    end if
  end block
  
  ! Performance metrics
  block
    real(real64) :: gflops
    gflops = real(2_int64 * m * n * k, real64) / ((end_time - start_time) * 1.0e9_real64)
    print *, ""
    print '(A,F8.3,A)', "Performance: ", gflops, " GFLOPS"
  end block
  
  ! Cleanup
  call destroy_memory(a_mem)
  call destroy_memory(b_mem)
  call destroy_memory(c_mem)
  
  print *, ""
  print *, "🎉 Matrix kernel test complete!"

contains

  ! Pure Fortran matrix multiplication kernel
  ! In real use, this would be tiled and optimized
  subroutine matmul_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: a(:,:), b(:,:), c(:,:)
    integer :: i, j, kk
    integer :: m_size, n_size, k_size
    
    print *, "DEBUG: In matmul_kernel, num args:", size(args)
    do i = 1, size(args)
      print *, "  Arg", i, "shape:", args(i)%shape
    end do
    
    ! Get matrix pointers and dimensions
    call c_f_pointer(args(1)%data%ptr, a, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, b, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, c, args(3)%shape)
    
    m_size = int(args(1)%shape(1))
    k_size = int(args(1)%shape(2))
    n_size = int(args(2)%shape(2))
    
    ! Simple matrix multiplication
    ! A real implementation would tile this for cache efficiency
    do j = 1, n_size
      do i = 1, m_size
        c(i,j) = 0.0_real32
        do kk = 1, k_size
          c(i,j) = c(i,j) + a(i,kk) * b(kk,j)
        end do
      end do
    end do
    
  end subroutine matmul_kernel

end program test_matrix_kernel