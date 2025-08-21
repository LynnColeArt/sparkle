program test_mesh
  use sparkle_mesh_types
  use sparkle_discovery
  implicit none
  
  type(mesh_topology) :: mesh
  type(link_metrics) :: link
  integer :: i
  
  print *, "=== Testing Sparkle Mesh Discovery ==="
  print *, "Building the foundation for distributed AI..."
  print *, ""
  
  ! Discover devices
  mesh = scan_devices()
  
  ! Show what we found
  call explain_devices(mesh)
  
  ! Profile interconnects
  print *, "Profiling device interconnects..."
  call profile_links(mesh)
  print *, "Profile complete!"
  print *, ""
  
  ! Show link metrics
  print *, "=== Interconnect Topology ==="
  if (allocated(mesh%links)) then
    do i = 1, min(size(mesh%links), 5)  ! Show first 5 links
      associate(l => mesh%links(i))
        if (l%src_id /= l%dst_id) then  ! Skip self-links
          print '(A,I0,A,I0,A)', "Link ", l%src_id, " -> ", l%dst_id, ":"
          print '(A,F0.1,A)', "  Bandwidth: ", l%bw_gbs, " GB/s"
          print '(A,F0.1,A)', "  Latency: ", l%latency_us, " μs"
          print '(A,L1)', "  Direct: ", l%direct
          print '(A,I0)', "  Hops: ", l%hops
          print *, ""
        end if
      end associate
    end do
  end if
  
  print *, "=== Ready to build the people's compute mesh! ==="
  
end program test_mesh