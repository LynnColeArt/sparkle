program test_nvidia_discovery
  ! Test NVIDIA GPU auto-detection in Sporkle
  
  use sporkle_discovery
  use sporkle_mesh_types
  implicit none
  
  type(mesh_topology) :: mesh
  integer :: i
  logical :: found_nvidia = .false.
  
  print *, "========================================="
  print *, "Sporkle NVIDIA Auto-Detection Test"
  print *, "========================================="
  print *, ""
  
  ! Scan for all devices
  mesh = scan_devices()
  
  print *, ""
  print *, "Device Discovery Complete!"
  print *, "--------------------------"
  
  ! Display all discovered devices
  call explain_devices(mesh)
  
  ! Check if we found NVIDIA GPU
  do i = 1, mesh%num_devices
    if (mesh%devices(i)%caps%kind == KIND_NVIDIA) then
      found_nvidia = .true.
      print *, "✅ NVIDIA GPU detected and added to mesh!"
      print *, "   Device ID:", mesh%devices(i)%id
      print *, "   Model:", trim(mesh%devices(i)%caps%driver_ver)
      print *, "   SMs:", mesh%devices(i)%caps%cores
      print *, "   VRAM:", mesh%devices(i)%caps%vram_mb, "MB"
      print *, "   Peak:", mesh%devices(i)%caps%peak_gflops, "GFLOPS"
    end if
  end do
  
  if (.not. found_nvidia) then
    print *, "❌ No NVIDIA GPU found in mesh"
  end if
  
  print *, ""
  print *, "========================================="
  print *, "Auto-detection test complete!"
  print *, "========================================="
  
end program test_nvidia_discovery