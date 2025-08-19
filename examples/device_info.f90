! device_info.f90 - Display information about available compute devices
program device_info
  use sporkle_mesh_types
  use sporkle_discovery
  implicit none
  
  type(mesh_topology) :: mesh
  integer :: i
  
  print *, "========================================="
  print *, "      Sporkle Device Information         "
  print *, "========================================="
  print *, ""
  
  ! Discover all available devices
  mesh = scan_devices()
  
  ! Display detailed information
  call explain_devices(mesh)
  
  ! Show device connectivity potential
  if (mesh%num_devices > 1) then
    print *, "Potential Device Connections:"
    do i = 1, mesh%num_devices
      print '(A,I0,A,A,A)', "  Device ", mesh%devices(i)%id, &
            " (", trim(mesh%devices(i)%caps%kind), ") ready for mesh participation"
    end do
  else
    print *, "Single device system - perfect for contributing to larger mesh!"
  end if
  
  print *, ""
  print *, "Every device can contribute to the people's AI infrastructure!"
  
end program device_info