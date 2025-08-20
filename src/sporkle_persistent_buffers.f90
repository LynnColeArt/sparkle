module sporkle_persistent_buffers
  ! Persistent, coherent GPU buffer mapping for maximum async throughput
  ! GL_ARB_buffer_storage with GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT
  
  use kinds
  use iso_c_binding
  implicit none
  
  private
  public :: persistent_buffers_init, persistent_buffers_get_next
  public :: persistent_buffers_shutdown, buffer_ring
  
  ! OpenGL constants for persistent mapping
  integer(c_int), parameter :: GL_BUFFER_STORAGE = int(z'8A2F', c_int)
  integer(c_int), parameter :: GL_MAP_PERSISTENT_BIT = int(z'0040', c_int)
  integer(c_int), parameter :: GL_MAP_COHERENT_BIT = int(z'0080', c_int)
  integer(c_int), parameter :: GL_MAP_WRITE_BIT = int(z'0002', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_STORAGE_BIT = int(z'0100', c_int)
  
  ! Ring buffer configuration
  integer, parameter :: RING_SIZE = 4  ! Quad buffering for maximum async
  
  ! Buffer ring for persistent mapping
  type :: buffer_ring
    integer :: input_buffer(RING_SIZE) = 0
    integer :: kernel_buffer(RING_SIZE) = 0  
    integer :: output_buffer(RING_SIZE) = 0
    type(c_ptr) :: input_ptr(RING_SIZE) = c_null_ptr
    type(c_ptr) :: kernel_ptr(RING_SIZE) = c_null_ptr
    type(c_ptr) :: output_ptr(RING_SIZE) = c_null_ptr
    type(c_ptr) :: fence(RING_SIZE) = c_null_ptr
    logical :: in_use(RING_SIZE) = .false.
    integer :: current_slot = 1
    integer(c_size_t) :: input_size = 0
    integer(c_size_t) :: kernel_size = 0
    integer(c_size_t) :: output_size = 0
  end type buffer_ring
  
  type(buffer_ring), save :: ring
  logical :: buffers_initialized = .false.
  
  ! OpenGL function interfaces
  interface
    subroutine glGenBuffers(n, buffers) bind(C, name='glGenBuffers')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: buffers
    end subroutine
    
    subroutine glBindBuffer(target, buffer) bind(C, name='glBindBuffer')
      import :: c_int
      integer(c_int), value :: target, buffer
    end subroutine
    
    subroutine glBufferStorage(target, size, data, flags) bind(C, name='glBufferStorage')
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: size
      type(c_ptr), value :: data
      integer(c_int), value :: flags
    end subroutine
    
    function glMapBufferRange(target, offset, length, access) bind(C, name='glMapBufferRange')
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: offset, length
      integer(c_int), value :: access
      type(c_ptr) :: glMapBufferRange
    end function
    
    function glFenceSync(condition, flags) bind(C, name='glFenceSync')
      import :: c_int, c_ptr
      integer(c_int), value :: condition, flags
      type(c_ptr) :: glFenceSync
    end function
    
    function glClientWaitSync(sync, flags, timeout) bind(C, name='glClientWaitSync')
      import :: c_int, c_ptr, c_int64_t
      type(c_ptr), value :: sync
      integer(c_int), value :: flags
      integer(c_int64_t), value :: timeout
      integer(c_int) :: glClientWaitSync
    end function
    
    subroutine glDeleteSync(sync) bind(C, name='glDeleteSync')
      import :: c_ptr
      type(c_ptr), value :: sync
    end subroutine
    
    subroutine glDeleteBuffers(n, buffers) bind(C, name='glDeleteBuffers')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: buffers
    end subroutine
  end interface
  
  ! Sync constants
  integer(c_int), parameter :: GL_SYNC_GPU_COMMANDS_COMPLETE = int(z'9117', c_int)
  integer(c_int), parameter :: GL_ALREADY_SIGNALED = int(z'911A', c_int)
  integer(c_int), parameter :: GL_CONDITION_SATISFIED = int(z'911C', c_int)
  integer(c_int), parameter :: GL_SHADER_STORAGE_BUFFER = int(z'90D2', c_int)
  
contains

  function persistent_buffers_init(max_input_size, max_kernel_size, max_output_size) result(success)
    integer(c_size_t), intent(in) :: max_input_size, max_kernel_size, max_output_size
    logical :: success
    
    integer :: slot
    integer(c_int), target :: buffer_ids(3)
    integer(c_int) :: storage_flags
    
    success = .false.
    
    if (buffers_initialized) then
      success = .true.
      return
    end if
    
    print *, "=== Persistent Coherent Buffer Ring Initialization ==="
    print *, "Ring size: 4 buffers (quad buffering)"
    print *, "Mapping: GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT"
    print *, ""
    
    ! Store buffer sizes
    ring%input_size = max_input_size
    ring%kernel_size = max_kernel_size
    ring%output_size = max_output_size
    
    ! Storage flags for persistent, coherent mapping
    storage_flags = ior(ior(GL_MAP_PERSISTENT_BIT, GL_MAP_COHERENT_BIT), &
                        ior(GL_MAP_WRITE_BIT, GL_DYNAMIC_STORAGE_BIT))
    
    ! Create quad ring buffers
    do slot = 1, RING_SIZE
      ! Generate 3 buffers per slot
      call glGenBuffers(3, c_loc(buffer_ids))
      
      ring%input_buffer(slot) = buffer_ids(1)
      ring%kernel_buffer(slot) = buffer_ids(2)
      ring%output_buffer(slot) = buffer_ids(3)
      
      ! Create and map input buffer
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ring%input_buffer(slot))
      call glBufferStorage(GL_SHADER_STORAGE_BUFFER, max_input_size, c_null_ptr, storage_flags)
      ring%input_ptr(slot) = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, &
                                             max_input_size, storage_flags)
      
      ! Create and map kernel buffer
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ring%kernel_buffer(slot))
      call glBufferStorage(GL_SHADER_STORAGE_BUFFER, max_kernel_size, c_null_ptr, storage_flags)
      ring%kernel_ptr(slot) = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_size_t, &
                                              max_kernel_size, storage_flags)
      
      ! Create output buffer (write-only, no mapping needed for results)
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, ring%output_buffer(slot))
      call glBufferStorage(GL_SHADER_STORAGE_BUFFER, max_output_size, c_null_ptr, &
                          GL_DYNAMIC_STORAGE_BIT)
      
      ring%in_use(slot) = .false.
      ring%fence(slot) = c_null_ptr
      
      print '(A,I0,A,I0,A,I0,A,I0)', "Buffer slot ", slot, ": input=", buffer_ids(1), &
             ", kernel=", buffer_ids(2), ", output=", buffer_ids(3)
      
      if (.not. c_associated(ring%input_ptr(slot)) .or. &
          .not. c_associated(ring%kernel_ptr(slot))) then
        print *, "ERROR: Failed to map persistent buffers for slot ", slot
        return
      end if
    end do
    
    ring%current_slot = 1
    buffers_initialized = .true.
    success = .true.
    
    print *, ""
    print *, "✓ Persistent buffer ring ready"
    print *, "  4× persistent, coherent mapped buffers"
    print *, "  Zero-copy CPU→GPU data transfer"
    print *, "  Maximum async throughput enabled"
    print *, ""
    
  end function persistent_buffers_init
  
  function persistent_buffers_get_next() result(slot)
    integer :: slot
    integer :: attempts, check_slot
    
    if (.not. buffers_initialized) then
      print *, "ERROR: Persistent buffers not initialized"
      slot = -1
      return
    end if
    
    ! Try current slot first
    slot = ring%current_slot
    if (.not. ring%in_use(slot)) then
      ring%in_use(slot) = .true.
      return
    end if
    
    ! Check if current slot just finished (non-blocking)
    if (c_associated(ring%fence(slot))) then
      if (is_fence_signaled(slot)) then
        call cleanup_slot(slot)
        ring%in_use(slot) = .true.
        return
      end if
    end if
    
    ! Find next available slot
    do attempts = 1, RING_SIZE
      ring%current_slot = mod(ring%current_slot, RING_SIZE) + 1
      check_slot = ring%current_slot
      
      if (.not. ring%in_use(check_slot)) then
        slot = check_slot
        ring%in_use(slot) = .true.
        return
      end if
      
      ! Check if this slot just finished
      if (c_associated(ring%fence(check_slot))) then
        if (is_fence_signaled(check_slot)) then
          call cleanup_slot(check_slot)
          slot = check_slot
          ring%in_use(slot) = .true.
          return
        end if
      end if
    end do
    
    ! All slots busy - this should be rare with quad buffering
    print *, "Warning: All buffer slots busy, waiting for slot 1"
    call wait_for_slot(1)
    slot = 1
    ring%in_use(slot) = .true.
    ring%current_slot = 1
    
  end function persistent_buffers_get_next
  
  function is_fence_signaled(slot) result(signaled)
    integer, intent(in) :: slot
    logical :: signaled
    integer(c_int) :: result
    
    signaled = .false.
    
    if (.not. c_associated(ring%fence(slot))) then
      signaled = .true.
      return
    end if
    
    ! Non-blocking check (0 timeout)
    result = glClientWaitSync(ring%fence(slot), 0, 0_c_int64_t)
    signaled = (result == GL_ALREADY_SIGNALED .or. result == GL_CONDITION_SATISFIED)
    
  end function is_fence_signaled
  
  subroutine cleanup_slot(slot)
    integer, intent(in) :: slot
    
    if (c_associated(ring%fence(slot))) then
      call glDeleteSync(ring%fence(slot))
      ring%fence(slot) = c_null_ptr
    end if
    ring%in_use(slot) = .false.
    
  end subroutine cleanup_slot
  
  subroutine wait_for_slot(slot)
    integer, intent(in) :: slot
    integer(c_int) :: result
    
    if (c_associated(ring%fence(slot))) then
      result = glClientWaitSync(ring%fence(slot), 0, 1000000000_c_int64_t)  ! 1 second
      call cleanup_slot(slot)
    end if
    
  end subroutine wait_for_slot
  
  subroutine persistent_buffers_set_fence(slot, fence_obj)
    integer, intent(in) :: slot
    type(c_ptr), intent(in) :: fence_obj
    
    if (slot >= 1 .and. slot <= RING_SIZE) then
      ring%fence(slot) = fence_obj
    end if
    
  end subroutine persistent_buffers_set_fence
  
  subroutine persistent_buffers_shutdown()
    integer :: slot
    integer(c_int), target :: buffer_ids(3)
    
    if (.not. buffers_initialized) return
    
    print *, "Shutting down persistent buffer ring..."
    
    do slot = 1, RING_SIZE
      ! Wait for any pending work
      if (ring%in_use(slot)) then
        call wait_for_slot(slot)
      end if
      
      ! Delete buffers
      if (ring%input_buffer(slot) /= 0) then
        buffer_ids(1) = ring%input_buffer(slot)
        buffer_ids(2) = ring%kernel_buffer(slot)
        buffer_ids(3) = ring%output_buffer(slot)
        call glDeleteBuffers(3, c_loc(buffer_ids))
      end if
      
      ring%input_buffer(slot) = 0
      ring%kernel_buffer(slot) = 0
      ring%output_buffer(slot) = 0
      ring%input_ptr(slot) = c_null_ptr
      ring%kernel_ptr(slot) = c_null_ptr
    end do
    
    buffers_initialized = .false.
    print *, "✓ Persistent buffer ring shut down"
    
  end subroutine persistent_buffers_shutdown

end module sporkle_persistent_buffers