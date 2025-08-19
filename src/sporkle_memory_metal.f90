module sporkle_memory_metal
  ! Metal-backed memory pool implementation
  ! The Sporkle Way: Unified memory done right!
  
  use kinds
  use iso_c_binding
  use sporkle_types
  use sporkle_memory
  use sporkle_gpu_metal
  use sporkle_error_handling, only: sporkle_error_sub => sporkle_error
  implicit none
  private
  
  public :: metal_memory_pool
  public :: create_metal_pool, destroy_metal_pool
  public :: metal_pool_allocate, metal_pool_deallocate
  public :: metal_pool_get_buffer
  
  ! Allocation info for tracking
  type :: metal_allocation
    type(metal_buffer) :: buffer           ! The actual Metal buffer
    integer(i64) :: size = 0             ! Size in bytes
    integer(i64) :: requested_size = 0   ! Original requested size
    logical :: in_use = .false.            ! Currently allocated?
    character(len=64) :: tag = ""          ! Debug tag
    real(dp) :: last_used = 0.0        ! For LRU eviction
  end type metal_allocation
  
  ! Metal-backed memory pool
  type :: metal_memory_pool
    type(metal_context), pointer :: ctx => null()
    type(metal_allocation), allocatable :: allocations(:)
    integer :: max_allocations = 1024
    integer :: num_allocations = 0
    integer(i64) :: total_allocated = 0
    integer(i64) :: peak_allocated = 0
    integer(i64) :: current_allocated = 0
    logical :: initialized = .false.
    
    ! Pool configuration
    logical :: use_shared_memory = .true.  ! Use MTLResourceStorageModeShared
    logical :: enable_caching = .true.     ! Reuse freed buffers
    integer(i64) :: min_block_size = 1024 * 1024  ! 1 MB minimum
    
    ! Statistics
    integer :: cache_hits = 0
    integer :: cache_misses = 0
    
  contains
    procedure :: get_stats => metal_pool_stats
    procedure :: defragment => metal_pool_defragment
  end type metal_memory_pool
  
contains

  ! Create a Metal-backed memory pool
  function create_metal_pool(ctx, max_size) result(pool)
    type(metal_context), target, intent(in) :: ctx
    integer(i64), intent(in), optional :: max_size
    type(metal_memory_pool) :: pool
    
    if (.not. ctx%initialized) then
      call sporkle_error_sub("Metal context not initialized", .false.)
      return
    end if
    
    pool%ctx => ctx
    pool%initialized = .true.
    
    if (present(max_size)) then
      pool%max_allocations = int(max_size / (1024 * 1024))  ! Assume 1MB average
    end if
    
    allocate(pool%allocations(pool%max_allocations))
    
    print *, "🎯 Created Metal memory pool:"
    print '(A,I0)', "   Max allocations: ", pool%max_allocations
    print '(A,L1)', "   Shared memory: ", pool%use_shared_memory
    print '(A,L1)', "   Caching enabled: ", pool%enable_caching
    
  end function create_metal_pool
  
  ! Allocate from Metal pool
  function metal_pool_allocate(pool, size, tag) result(handle)
    type(metal_memory_pool), intent(inout) :: pool
    integer(i64), intent(in) :: size
    character(len=*), intent(in), optional :: tag
    type(memory_handle) :: handle
    
    integer :: i, slot
    integer(i64) :: alloc_size
    logical :: found_cached
    
    if (.not. pool%initialized) then
      call sporkle_error_sub("Metal pool not initialized", .false.)
      handle%is_allocated = .false.
      return
    end if
    
    ! Round up to minimum block size for efficiency
    alloc_size = max(size, pool%min_block_size)
    
    ! Try to find a cached buffer first
    slot = -1
    found_cached = .false.
    
    if (pool%enable_caching) then
      do i = 1, pool%num_allocations
        if (.not. pool%allocations(i)%in_use .and. &
            pool%allocations(i)%size >= size .and. &
            pool%allocations(i)%size <= alloc_size * 2) then
          ! Reuse this buffer
          slot = i
          found_cached = .true.
          pool%allocations(i)%in_use = .true.
          pool%allocations(i)%requested_size = size
          if (present(tag)) pool%allocations(i)%tag = tag
          pool%cache_hits = pool%cache_hits + 1
          exit
        end if
      end do
    end if
    
    ! Allocate new buffer if no cached one found
    if (slot < 0) then
      pool%cache_misses = pool%cache_misses + 1
      
      ! Find free slot
      do i = 1, pool%max_allocations
        if (pool%allocations(i)%size == 0) then
          slot = i
          exit
        end if
      end do
      
      if (slot < 0) then
        call sporkle_error_sub("Metal pool full - no free slots", .false.)
        handle%is_allocated = .false.
        return
      end if
      
      ! Create new Metal buffer
      pool%allocations(slot)%buffer = create_metal_buffer(pool%ctx, alloc_size)
      
      if (.not. pool%allocations(slot)%buffer%allocated) then
        call sporkle_error_sub("Failed to create Metal buffer", .false.)
        handle%is_allocated = .false.
        return
      end if
      
      pool%allocations(slot)%size = alloc_size
      pool%allocations(slot)%requested_size = size
      pool%allocations(slot)%in_use = .true.
      if (present(tag)) pool%allocations(slot)%tag = tag
      
      if (slot > pool%num_allocations) pool%num_allocations = slot
      pool%total_allocated = pool%total_allocated + alloc_size
      pool%current_allocated = pool%current_allocated + alloc_size
    else
      ! Using cached buffer
      pool%current_allocated = pool%current_allocated + pool%allocations(slot)%size
    end if
    
    ! Update peak tracking
    if (pool%current_allocated > pool%peak_allocated) then
      pool%peak_allocated = pool%current_allocated
    end if
    
    ! Create memory handle
    handle%ptr = pool%allocations(slot)%buffer%buffer  ! C pointer to MTLBuffer
    handle%size = size  ! Actual requested size
    handle%device_id = slot  ! Store slot for later lookup
    handle%is_allocated = .true.
    if (present(tag)) handle%tag = tag
    
    if (found_cached) then
      print '(A,F0.2,A)', "   💚 Allocated ", real(size) / real(1024**2), &
             " MB from cache"
    else
      print '(A,F0.2,A)', "   🆕 Allocated ", real(size) / real(1024**2), &
             " MB (new buffer)"
    end if
    
  end function metal_pool_allocate
  
  ! Deallocate from Metal pool  
  subroutine metal_pool_deallocate(pool, handle)
    type(metal_memory_pool), intent(inout) :: pool
    type(memory_handle), intent(inout) :: handle
    
    integer :: slot
    
    if (.not. handle%is_allocated) return
    
    ! Extract slot from device_id
    slot = handle%device_id
    
    if (slot < 1 .or. slot > pool%max_allocations) then
      call sporkle_error_sub("Invalid Metal pool slot", .false.)
      return
    end if
    
    ! Update allocated size tracking
    pool%current_allocated = pool%current_allocated - pool%allocations(slot)%size
    
    if (pool%enable_caching) then
      ! Mark as available for reuse
      pool%allocations(slot)%in_use = .false.
      print '(A,F0.2,A)', "   ♻️  Returned ", real(handle%size) / real(1024**2), &
             " MB to cache"
    else
      ! Actually free the buffer
      call destroy_metal_buffer(pool%allocations(slot)%buffer)
      pool%total_allocated = pool%total_allocated - pool%allocations(slot)%size
      pool%allocations(slot)%size = 0
      pool%allocations(slot)%in_use = .false.
      pool%allocations(slot)%buffer%allocated = .false.
      print '(A,F0.2,A)', "   🗑️  Freed ", real(handle%size) / real(1024**2), &
             " MB from Metal"
    end if
    
    handle%is_allocated = .false.
    handle%ptr = c_null_ptr
    
  end subroutine metal_pool_deallocate
  
  ! Get statistics
  subroutine metal_pool_stats(this)
    class(metal_memory_pool), intent(in) :: this
    integer :: i, cached_count, in_use_count
    integer(i64) :: cached_size, in_use_size
    real(sp) :: cache_hit_rate
    
    cached_count = 0
    cached_size = 0
    in_use_count = 0
    in_use_size = 0
    
    do i = 1, this%num_allocations
      if (this%allocations(i)%size > 0) then
        if (this%allocations(i)%in_use) then
          in_use_count = in_use_count + 1
          in_use_size = in_use_size + this%allocations(i)%size
        else
          cached_count = cached_count + 1
          cached_size = cached_size + this%allocations(i)%size
        end if
      end if
    end do
    
    if (this%cache_hits + this%cache_misses > 0) then
      cache_hit_rate = real(this%cache_hits) / real(this%cache_hits + this%cache_misses) * 100.0
    else
      cache_hit_rate = 0.0
    end if
    
    print *, "╔══════════════════════════════════════╗"
    print *, "║   Metal Memory Pool Statistics      ║"
    print *, "╚══════════════════════════════════════╝"
    print '(A,I0)', " Active allocations: ", in_use_count
    print '(A,F0.2,A)', " In use: ", real(in_use_size) / real(1024**2), " MB"
    print '(A,I0,A,F0.2,A)', " Cached: ", cached_count, &
           " buffers (", real(cached_size) / real(1024**2), " MB)"
    print '(A,F0.2,A)', " Total allocated: ", real(this%total_allocated) / real(1024**3), " GB"
    print '(A,F0.2,A)', " Peak allocated: ", real(this%peak_allocated) / real(1024**3), " GB"
    print '(A,F0.1,A)', " Cache hit rate: ", cache_hit_rate, "%"
    print '(A,I0,A,I0)', " Hits/Misses: ", this%cache_hits, " / ", this%cache_misses
    print *, ""
    
  end subroutine metal_pool_stats
  
  ! Defragment pool (consolidate free space)
  subroutine metal_pool_defragment(this)
    class(metal_memory_pool), intent(inout) :: this
    
    ! For Metal with unified memory, defragmentation isn't really needed
    ! The OS handles virtual memory efficiently
    print *, "ℹ️  Metal pool defragmentation not needed (unified memory)"
    
  end subroutine metal_pool_defragment
  
  ! Get Metal buffer from memory handle
  function metal_pool_get_buffer(pool, handle) result(buffer)
    type(metal_memory_pool), intent(in) :: pool
    type(memory_handle), intent(in) :: handle
    type(metal_buffer) :: buffer
    
    integer :: slot
    
    slot = handle%device_id
    if (slot < 1 .or. slot > pool%max_allocations) then
      buffer%allocated = .false.
      return
    end if
    
    buffer = pool%allocations(slot)%buffer
    
  end function metal_pool_get_buffer
  
  ! Destroy Metal pool
  subroutine destroy_metal_pool(pool)
    type(metal_memory_pool), intent(inout) :: pool
    integer :: i
    
    if (.not. pool%initialized) return
    
    print *, "🧹 Destroying Metal memory pool..."
    
    ! Free all Metal buffers
    do i = 1, pool%num_allocations
      if (pool%allocations(i)%buffer%allocated) then
        call destroy_metal_buffer(pool%allocations(i)%buffer)
      end if
    end do
    
    if (allocated(pool%allocations)) deallocate(pool%allocations)
    pool%initialized = .false.
    
    print '(A,F0.2,A)', "   Released ", real(pool%total_allocated) / real(1024**3), " GB total"
    
  end subroutine destroy_metal_pool
  
  ! Helper to destroy a Metal buffer
  subroutine destroy_metal_buffer(buffer)
    type(metal_buffer), intent(inout) :: buffer
    
    interface
      subroutine c_metal_destroy_buffer(buf) bind(C, name="metal_destroy_buffer")
        use iso_c_binding, only: c_ptr
        type(c_ptr), value :: buf
      end subroutine c_metal_destroy_buffer
    end interface
    
    if (buffer%allocated .and. c_associated(buffer%buffer)) then
      call c_metal_destroy_buffer(buffer%buffer)
      buffer%allocated = .false.
      buffer%buffer = c_null_ptr
    end if
    
  end subroutine destroy_metal_buffer

end module sporkle_memory_metal