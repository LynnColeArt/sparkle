program test_scheduler
  use sparkle_mesh_types
  use sparkle_discovery
  use sparkle_scheduler
  use kinds
  implicit none
  
  type(mesh_topology) :: mesh
  type(schedule_choice) :: schedule
  type(device_handle) :: fake_gpu1, fake_gpu2, old_laptop
  integer(i64) :: work_size
  
  print *, "=== Testing Sparkle Intelligent Scheduler ==="
  print *, ""
  
  ! Build a mesh with diverse devices
  mesh = scan_devices()  ! Gets CPU
  
  ! Add a fake high-end GPU
  fake_gpu1%id = 1
  fake_gpu1%caps%kind = KIND_NVIDIA
  fake_gpu1%caps%cores = 68  ! Like RTX 3080
  fake_gpu1%caps%vram_mb = 10240
  fake_gpu1%caps%peak_gflops = 29700.0_rk64  ! ~30 TFLOPS
  fake_gpu1%caps%sustained_gflops = 20000.0_rk64  ! ~67% efficiency
  fake_gpu1%caps%mem_bw_gbs = 760.0_rk64
  fake_gpu1%caps%unified_mem = .false.
  fake_gpu1%healthy = .true.
  fake_gpu1%load = 0.0
  call mesh%add_device(fake_gpu1)
  
  ! Add a fake mid-range GPU
  fake_gpu2%id = 2
  fake_gpu2%caps%kind = KIND_AMD
  fake_gpu2%caps%cores = 36
  fake_gpu2%caps%vram_mb = 8192
  fake_gpu2%caps%peak_gflops = 10000.0_rk64  ! 10 TFLOPS
  fake_gpu2%caps%sustained_gflops = 6000.0_rk64  ! 60% efficiency
  fake_gpu2%caps%mem_bw_gbs = 448.0_rk64
  fake_gpu2%caps%unified_mem = .false.
  fake_gpu2%healthy = .true.
  fake_gpu2%load = 0.3  ! 30% busy with other work
  call mesh%add_device(fake_gpu2)
  
  ! Add an old laptop (integrated graphics)
  old_laptop%id = 3
  old_laptop%caps%kind = KIND_IGPU
  old_laptop%caps%cores = 96  ! Intel Xe
  old_laptop%caps%vram_mb = 4096  ! Shared memory
  old_laptop%caps%peak_gflops = 1000.0_rk64  ! 1 TFLOP
  old_laptop%caps%sustained_gflops = 400.0_rk64  ! 40% efficiency
  old_laptop%caps%mem_bw_gbs = 68.0_rk64
  old_laptop%caps%unified_mem = .true.  ! Shares system RAM
  old_laptop%healthy = .true.
  old_laptop%load = 0.0
  call mesh%add_device(old_laptop)
  
  ! Profile the mesh
  call profile_links(mesh)
  
  ! Show our heterogeneous mesh
  call explain_devices(mesh)
  
  ! Test scheduling different workloads
  print *, "=== Scheduling Test 1: Large GEMM ==="
  work_size = 1000000000_i64  ! 1 billion elements
  schedule = plan_shards(mesh, work_size, "gemm")
  call explain_schedule(mesh, schedule)
  
  print *, "=== Scheduling Test 2: Small workload ==="
  work_size = 1000000_i64  ! 1 million elements
  schedule = plan_shards(mesh, work_size, "gemm")
  call explain_schedule(mesh, schedule)
  
  ! Simulate device failure
  print *, "=== Scheduling Test 3: GPU 1 fails ==="
  mesh%devices(2)%healthy = .false.  ! GPU 1 fails
  schedule = plan_shards(mesh, 1000000000_i64, "gemm")
  call explain_schedule(mesh, schedule)
  
  print *, "=== The people's AI infrastructure in action! ==="
  print *, "Every device contributes according to its ability."
  
end program test_scheduler
