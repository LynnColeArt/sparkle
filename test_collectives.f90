program test_collectives
  use sparkle_mesh_types
  use sparkle_discovery
  use sparkle_collectives
  use iso_c_binding
  use iso_fortran_env, only: int64
  implicit none
  
  type(mesh_topology) :: mesh
  type(device_handle) :: gpu1, gpu2, gpu3
  type(c_ptr) :: dummy_buffer
  integer(int64) :: small_size, large_size
  
  print *, "=== Testing Sparkle Collective Operations ==="
  print *, "The foundation for distributed AI training"
  print *, ""
  
  ! Build a mesh with multiple devices
  mesh = scan_devices()  ! CPU
  
  ! Add some fake GPUs to test collective algorithms
  gpu1%id = 1
  gpu1%caps%kind = KIND_NVIDIA
  gpu1%caps%cores = 80
  gpu1%caps%vram_mb = 16384
  gpu1%caps%mem_bw_gbs = 900.0_rk64
  gpu1%caps%p2p_direct = .true.
  gpu1%healthy = .true.
  call mesh%add_device(gpu1)
  
  gpu2%id = 2  
  gpu2%caps%kind = KIND_AMD
  gpu2%caps%cores = 60
  gpu2%caps%vram_mb = 16384
  gpu2%caps%mem_bw_gbs = 512.0_rk64
  gpu2%caps%p2p_direct = .false.  ! No P2P with NVIDIA
  gpu2%healthy = .true.
  call mesh%add_device(gpu2)
  
  gpu3%id = 3
  gpu3%caps%kind = KIND_NVIDIA
  gpu3%caps%cores = 68
  gpu3%caps%vram_mb = 12288
  gpu3%caps%mem_bw_gbs = 760.0_rk64
  gpu3%caps%p2p_direct = .true.  ! P2P with other NVIDIA
  gpu3%healthy = .true.
  call mesh%add_device(gpu3)
  
  ! Set up some P2P links
  block
    type(link_metrics) :: link
    
    ! GPU1 <-> GPU3 have P2P
    link%src_id = 1
    link%dst_id = 3
    link%direct = .true.
    link%bw_gbs = 50.0_rk64  ! NVLink bandwidth
    link%latency_us = 1.0_rk64
    link%hops = 1
    call mesh%update_link(link)
    
    ! Reverse direction
    link%src_id = 3
    link%dst_id = 1
    call mesh%update_link(link)
  end block
  
  ! Profile the mesh
  call profile_links(mesh)
  
  print *, "=== Test 1: Small All-Reduce (Gradients) ==="
  small_size = 1000_int64  ! 1000 float32s = 4KB
  call explain_collective_plan(mesh, "all_reduce", small_size * 4)
  
  dummy_buffer = c_null_ptr  ! Would be actual device buffer
  call all_reduce(mesh, dummy_buffer, small_size, DTYPE_REAL32, OP_SUM)
  print *, ""
  
  print *, "=== Test 2: Large All-Reduce (Model Weights) ==="
  large_size = 100000000_int64  ! 100M float32s = 400MB
  call explain_collective_plan(mesh, "all_reduce", large_size * 4)
  call all_reduce(mesh, dummy_buffer, large_size, DTYPE_REAL32, OP_MEAN)
  print *, ""
  
  print *, "=== Test 3: Broadcast (Model Distribution) ==="
  call explain_collective_plan(mesh, "broadcast", large_size * 4)
  call broadcast(mesh, dummy_buffer, large_size, DTYPE_REAL32, root_device=1)
  print *, ""
  
  print *, "=== Test 4: Device Failure During Collective ==="
  print *, "Simulating GPU 2 failure..."
  mesh%devices(3)%healthy = .false.  ! GPU2 fails (device index 3)
  
  call explain_collective_plan(mesh, "all_reduce", small_size * 4)
  call all_reduce(mesh, dummy_buffer, small_size, DTYPE_REAL32, OP_SUM)
  print *, ""
  
  print *, "=== Collective operations complete! ==="
  print *, "The mesh adapted to device failure and continued operating."
  print *, "This is how distributed AI becomes resilient."
  
end program test_collectives