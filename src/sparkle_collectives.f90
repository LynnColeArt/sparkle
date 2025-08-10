module sparkle_collectives
  use sparkle_mesh_types
  use iso_c_binding, only: c_ptr
  use iso_fortran_env, only: real32, real64, int32, int64
  implicit none
  private
  
  public :: all_reduce, broadcast, explain_collective_plan
  public :: DTYPE_REAL32, DTYPE_REAL64, DTYPE_INT32, DTYPE_INT64
  
  ! Data type constants
  integer, parameter :: DTYPE_REAL32 = 1
  integer, parameter :: DTYPE_REAL64 = 2
  integer, parameter :: DTYPE_INT32 = 3
  integer, parameter :: DTYPE_INT64 = 4
  
  ! Collective algorithms
  integer, parameter :: ALGO_RING = 1
  integer, parameter :: ALGO_TREE = 2
  integer, parameter :: ALGO_DIRECT = 3
  
contains

  ! All-reduce operation across mesh
  subroutine all_reduce(mesh, buffer, count, dtype, op, root_device)
    type(mesh_topology), intent(in) :: mesh
    type(c_ptr), intent(inout) :: buffer        ! Device buffer pointer
    integer(int64), intent(in) :: count         ! Number of elements
    integer, intent(in) :: dtype                ! Data type (real32/real64/etc)
    integer, intent(in) :: op                   ! Operation (OP_SUM, OP_MAX, etc)
    integer, intent(in), optional :: root_device
    
    integer :: algorithm
    integer(int64) :: bytes_per_element
    character(len=100) :: plan_description
    
    ! Determine bytes per element
    ! Using simple integer codes for data types
    select case(dtype)
      case(1)  ! real32
        bytes_per_element = 4
      case(2)  ! real64
        bytes_per_element = 8
      case(3)  ! int32
        bytes_per_element = 4
      case(4)  ! int64
        bytes_per_element = 8
      case default
        bytes_per_element = 8
    end select
    
    ! Choose algorithm based on mesh size and data size
    algorithm = choose_collective_algorithm(mesh, count * bytes_per_element)
    
    ! Execute chosen algorithm
    select case(algorithm)
      case(ALGO_RING)
        call all_reduce_ring(mesh, buffer, count, dtype, op)
        plan_description = "Ring algorithm (bandwidth-optimized for large data)"
        
      case(ALGO_TREE)
        call all_reduce_tree(mesh, buffer, count, dtype, op, root_device)
        plan_description = "Tree algorithm (latency-optimized for small data)"
        
      case(ALGO_DIRECT)
        call all_reduce_direct(mesh, buffer, count, dtype, op)
        plan_description = "Direct P2P (available due to mesh topology)"
        
      case default
        ! Fallback to ring
        call all_reduce_ring(mesh, buffer, count, dtype, op)
        plan_description = "Ring algorithm (fallback)"
    end select
    
    print '(A,A)', "All-reduce completed using: ", trim(plan_description)
    
  end subroutine all_reduce
  
  ! Choose optimal collective algorithm
  function choose_collective_algorithm(mesh, total_bytes) result(algorithm)
    type(mesh_topology), intent(in) :: mesh
    integer(int64), intent(in) :: total_bytes
    integer :: algorithm
    
    integer :: num_devices, num_p2p_links
    logical :: all_p2p
    
    ! Count healthy devices
    num_devices = 0
    num_p2p_links = 0
    all_p2p = .true.
    
    block
      integer :: i, j
      do i = 1, mesh%num_devices
        if (mesh%devices(i)%healthy) num_devices = num_devices + 1
      end do
      
      ! Check P2P connectivity
      if (allocated(mesh%links)) then
        do i = 1, size(mesh%links)
          if (mesh%links(i)%direct) num_p2p_links = num_p2p_links + 1
          if (.not. mesh%links(i)%direct .and. &
              mesh%links(i)%src_id /= mesh%links(i)%dst_id) then
            all_p2p = .false.
          end if
        end do
      end if
    end block
    
    ! Decision tree
    if (num_devices <= 1) then
      algorithm = ALGO_DIRECT  ! No collective needed
    else if (all_p2p .and. num_devices <= 4) then
      algorithm = ALGO_DIRECT  ! Full P2P mesh for small clusters
    else if (total_bytes < 1024 * 1024) then  ! < 1MB
      algorithm = ALGO_TREE    ! Low latency for small messages
    else
      algorithm = ALGO_RING    ! High bandwidth for large messages
    end if
    
  end function choose_collective_algorithm
  
  ! Ring all-reduce implementation
  subroutine all_reduce_ring(mesh, buffer, count, dtype, op)
    type(mesh_topology), intent(in) :: mesh
    type(c_ptr), intent(inout) :: buffer
    integer(int64), intent(in) :: count
    integer, intent(in) :: dtype
    integer, intent(in) :: op
    
    integer :: ring_size, step
    integer, allocatable :: ring_order(:)
    integer :: my_rank, next_rank, prev_rank
    
    ! Build ring order from healthy devices
    ring_size = 0
    allocate(ring_order(mesh%num_devices))
    
    do step = 1, mesh%num_devices
      if (mesh%devices(step)%healthy) then
        ring_size = ring_size + 1
        ring_order(ring_size) = mesh%devices(step)%id
      end if
    end do
    
    if (ring_size <= 1) return  ! No collective needed
    
    print '(A,I0,A)', "Executing ring all-reduce across ", ring_size, " devices"
    
    ! Ring algorithm: 
    ! 1. Reduce-scatter: each device gets partial sum
    ! 2. All-gather: share partial sums around ring
    
    ! For demo purposes, we'll simulate the communication pattern
    do step = 1, ring_size - 1
      ! Each device sends to next, receives from previous
      my_rank = mod(step, ring_size) + 1
      next_rank = mod(my_rank, ring_size) + 1
      prev_rank = mod(my_rank - 2 + ring_size, ring_size) + 1
      
      print '(A,I0,A,I0,A,I0)', "  Step ", step, ": Device ", &
            ring_order(my_rank), " -> Device ", ring_order(next_rank)
    end do
    
    deallocate(ring_order)
    
  end subroutine all_reduce_ring
  
  ! Tree all-reduce implementation
  subroutine all_reduce_tree(mesh, buffer, count, dtype, op, root)
    type(mesh_topology), intent(in) :: mesh
    type(c_ptr), intent(inout) :: buffer
    integer(int64), intent(in) :: count
    integer, intent(in) :: dtype
    integer, intent(in) :: op
    integer, intent(in), optional :: root
    
    integer :: root_id, level, stride
    integer :: num_devices
    
    root_id = 0
    if (present(root)) root_id = root
    
    ! Count devices
    num_devices = 0
    do level = 1, mesh%num_devices
      if (mesh%devices(level)%healthy) num_devices = num_devices + 1
    end do
    
    print '(A,I0,A,I0)', "Executing tree all-reduce with root device ", &
          root_id, ", height ", ceiling(log(real(num_devices))/log(2.0))
    
    ! Tree reduction pattern
    stride = 1
    do while (stride < num_devices)
      print '(A,I0)', "  Tree level with stride ", stride
      stride = stride * 2
    end do
    
  end subroutine all_reduce_tree
  
  ! Direct P2P all-reduce (for small full-mesh clusters)
  subroutine all_reduce_direct(mesh, buffer, count, dtype, op)
    type(mesh_topology), intent(in) :: mesh
    type(c_ptr), intent(inout) :: buffer
    integer(int64), intent(in) :: count
    integer, intent(in) :: dtype
    integer, intent(in) :: op
    
    print *, "Executing direct P2P all-reduce (all-to-all)"
    
  end subroutine all_reduce_direct
  
  ! Broadcast operation
  subroutine broadcast(mesh, buffer, count, dtype, root_device)
    type(mesh_topology), intent(in) :: mesh
    type(c_ptr), intent(inout) :: buffer
    integer(int64), intent(in) :: count
    integer, intent(in) :: dtype
    integer, intent(in) :: root_device
    
    integer :: algorithm
    integer(int64) :: bytes_per_element
    
    ! Determine bytes
    select case(dtype)
      case(1)  ! real32
        bytes_per_element = 4
      case(2)  ! real64
        bytes_per_element = 8
      case default
        bytes_per_element = 8
    end select
    
    ! For broadcast, tree is usually optimal
    algorithm = ALGO_TREE
    
    print '(A,I0,A,I0)', "Broadcasting from device ", root_device, &
          " to ", mesh%num_devices - 1, " devices"
    
    ! Tree broadcast from root
    call broadcast_tree(mesh, buffer, count, dtype, root_device)
    
  end subroutine broadcast
  
  ! Tree broadcast implementation
  subroutine broadcast_tree(mesh, buffer, count, dtype, root)
    type(mesh_topology), intent(in) :: mesh
    type(c_ptr), intent(inout) :: buffer
    integer(int64), intent(in) :: count
    integer, intent(in) :: dtype
    integer, intent(in) :: root
    
    integer :: level, stride, i
    
    ! Binary tree broadcast pattern
    stride = 1
    level = 0
    do while (stride < mesh%num_devices)
      level = level + 1
      print '(A,I0,A)', "  Broadcast level ", level, ":"
      
      ! At each level, nodes that have data send to nodes that don't
      do i = 0, mesh%num_devices - 1, stride * 2
        if (i + stride < mesh%num_devices) then
          print '(A,I0,A,I0)', "    Device ", i, " -> Device ", i + stride
        end if
      end do
      
      stride = stride * 2
    end do
    
  end subroutine broadcast_tree
  
  ! Explain collective operation plan
  subroutine explain_collective_plan(mesh, op_type, data_size)
    type(mesh_topology), intent(in) :: mesh
    character(len=*), intent(in) :: op_type
    integer(int64), intent(in) :: data_size
    
    integer :: algorithm
    real(rk64) :: size_mb
    
    size_mb = real(data_size, rk64) / (1024.0_rk64 * 1024.0_rk64)
    algorithm = choose_collective_algorithm(mesh, data_size)
    
    print *, "=== Collective Operation Plan ==="
    print '(A,A)', "Operation: ", op_type
    print '(A,F0.1,A)', "Data size: ", size_mb, " MB"
    print '(A,I0)', "Participating devices: ", mesh%num_devices
    
    select case(algorithm)
      case(ALGO_RING)
        print *, "Algorithm: RING"
        print *, "  - Optimal for large data (bandwidth-limited)"
        print *, "  - Gracefully handles non-uniform bandwidth"
        print *, "  - Time complexity: O(n) steps"
        
      case(ALGO_TREE)
        print *, "Algorithm: TREE"
        print *, "  - Optimal for small data (latency-limited)"
        print *, "  - Minimizes communication rounds"
        print *, "  - Time complexity: O(log n) steps"
        
      case(ALGO_DIRECT)
        print *, "Algorithm: DIRECT P2P"
        print *, "  - Full mesh connectivity available"
        print *, "  - Single-step all-to-all communication"
        print *, "  - Time complexity: O(1) steps"
    end select
    
    print *, ""
    
  end subroutine explain_collective_plan
  
end module sparkle_collectives