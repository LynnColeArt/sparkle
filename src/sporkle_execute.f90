module sporkle_execute
  use kinds
  use sporkle_types
  use sporkle_memory
  use sporkle_mesh_types
  use sporkle_kernels
  use sporkle_scheduler
  implicit none
  private
  
  public :: sporkle_run, sporkle_run_async, sporkle_run_quiet
  public :: execution_context, create_context
  
  type :: execution_context
    type(mesh_topology), pointer :: mesh => null()
    logical :: async_mode = .false.
    logical :: quiet_mode = .false.  ! Suppress output during benchmarks
    integer :: preferred_device = -1  ! -1 means auto-select
  contains
    procedure :: execute => context_execute_kernel
  end type execution_context
  
contains

  ! Create execution context
  function create_context(mesh) result(ctx)
    type(mesh_topology), target, intent(in) :: mesh
    type(execution_context) :: ctx
    
    ctx%mesh => mesh
    ctx%async_mode = .false.
    ctx%preferred_device = -1
    
  end function create_context
  
  ! Simple synchronous execution
  subroutine sporkle_run(kernel, mesh)
    type(sporkle_kernel), intent(inout) :: kernel
    type(mesh_topology), intent(inout) :: mesh
    
    type(execution_context) :: ctx
    
    ctx = create_context(mesh)
    call ctx%execute(kernel)
    
  end subroutine sporkle_run
  
  ! Asynchronous execution (returns immediately)
  subroutine sporkle_run_async(kernel, mesh)
    type(sporkle_kernel), intent(inout) :: kernel
    type(mesh_topology), intent(inout) :: mesh
    
    type(execution_context) :: ctx
    
    ctx = create_context(mesh)
    ctx%async_mode = .true.
    call ctx%execute(kernel)
    
  end subroutine sporkle_run_async
  
  ! Quiet execution (for benchmarking)
  subroutine sporkle_run_quiet(kernel, mesh)
    type(sporkle_kernel), intent(inout) :: kernel
    type(mesh_topology), intent(inout) :: mesh
    
    type(execution_context) :: ctx
    
    ctx = create_context(mesh)
    ctx%quiet_mode = .true.
    call ctx%execute(kernel)
    
  end subroutine sporkle_run_quiet
  
  ! Main execution logic
  subroutine context_execute_kernel(this, kernel)
    class(execution_context), intent(inout) :: this
    type(sporkle_kernel), intent(inout) :: kernel
    
    type(schedule_choice) :: schedule
    integer :: i, device_idx
    integer(i64) :: offset, chunk_size
    
    ! Set argument shapes from memory handles
    call kernel_set_argument_shapes(kernel)
    
    ! Validate kernel
    if (.not. kernel%validate()) then
      print *, "ERROR: Invalid kernel configuration"
      return
    end if
    
    ! Get execution plan
    schedule = plan_shards(this%mesh, kernel%work_items, "compute")
    
    if (.not. this%quiet_mode) then
      print '(A,A,A)', "🚀 Executing kernel '", kernel%name, "'"
      print '(A,I0,A)', "   Distributing ", kernel%work_items, " work items"
    end if
    
    ! Check if we can run on GPU
    block
      integer :: j
      logical :: has_gpu
      
      has_gpu = .false.
      do j = 1, this%mesh%num_devices
        if (this%mesh%devices(j)%caps%kind /= KIND_CPU) then
          has_gpu = .true.
          exit
        end if
      end do
      
      if (has_gpu .and. .not. this%quiet_mode) then
        print *, "   GPU execution not yet implemented - falling back to CPU"
      end if
    end block
    
    ! For now, execute on CPU devices only
    offset = 0
    do i = 1, size(schedule%device_ids)
      device_idx = schedule%device_ids(i) + 1  ! Convert to 1-based
      chunk_size = schedule%shards(i)
      
      if (device_idx <= this%mesh%num_devices) then
        if (this%mesh%devices(device_idx)%caps%kind == KIND_CPU) then
          ! Execute on CPU
          call execute_kernel_cpu(kernel, offset, chunk_size)
        else
          ! Skip GPU for now
          if (.not. this%quiet_mode) then
            print '(A,I0,A)', "   Skipping GPU device ", device_idx - 1, &
                            " (GPU execution coming soon)"
          end if
        end if
      end if
      
      offset = offset + chunk_size
    end do
    
    if (.not. this%quiet_mode) then
      print *, "✓ Kernel execution complete"
    end if
    
  end subroutine context_execute_kernel
  
  ! Execute kernel on CPU slice
  subroutine execute_kernel_cpu(kernel, offset, count)
    type(sporkle_kernel), intent(inout) :: kernel
    integer(i64), intent(in) :: offset, count
    
    ! For now, we'll create a simple wrapper that calls the Fortran procedure
    ! In a real implementation, this would:
    ! 1. Set up argument slices based on offset/count
    ! 2. Call the Fortran procedure
    ! 3. Handle any necessary synchronization
    
    if (associated(kernel%fortran_proc)) then
      ! Create views into the data for this chunk
      block
        type(kernel_argument), allocatable :: chunk_args(:)
        integer :: i
        
        ! For now, just pass through the original arguments
        ! In a real distributed implementation, we'd slice the data
        allocate(chunk_args(size(kernel%arguments)))
        chunk_args = kernel%arguments
        
        ! Call the actual Fortran procedure
        call kernel%fortran_proc(chunk_args)
      end block
    end if
    
  end subroutine execute_kernel_cpu
  
end module sporkle_execute