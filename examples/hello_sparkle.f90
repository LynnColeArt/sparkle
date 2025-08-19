! hello_sparkle.f90 - Your first Sporkle program!
program hello_sparkle
  use kinds
  use sporkle_mesh_types
  use sporkle_discovery
  use sporkle_scheduler
  use sporkle_memory
  implicit none
  
  type(mesh_topology) :: mesh
  type(memory_pool) :: pool
  type(memory_handle) :: data
  integer(i64) :: total_work = 1000000
  
  print *, "✨ Hello from Sporkle! ✨"
  print *, ""
  
  ! Discover all available compute devices
  print *, "🔍 Discovering devices..."
  mesh = scan_devices()
  call explain_devices(mesh)
  
  ! Create a memory pool
  print *, "💾 Setting up memory management..."
  call pool%init(100)
  data = pool%allocate(total_work * 4, tag="work_data")
  
  ! Distribute work across devices
  print *, "📊 Planning work distribution..."
  block
    type(schedule_choice) :: choice
    integer :: i
    
    choice = plan_shards(mesh, total_work, "compute_heavy")
    
    print *, ""
    print *, "Work distribution plan:"
    print *, "  Strategy: ", trim(choice%rationale)
    if (allocated(choice%device_ids)) then
      print *, "  Using ", size(choice%device_ids), " device(s)"
      do i = 1, size(choice%device_ids)
        print '(A,I0,A,I0,A)', "    Device ", choice%device_ids(i), &
              ": ", choice%shards(i), " work items"
      end do
    end if
  end block
  
  print *, ""
  print *, "🚀 Ready to democratize AI compute!"
  print *, ""
  print *, "Every device matters. Every cycle counts."
  print *, "Together, we build the people's AI infrastructure!"
  
  ! Cleanup
  call pool%cleanup()
  
end program hello_sparkle