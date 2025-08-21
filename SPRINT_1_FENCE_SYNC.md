# Sprint 1: Fence-Based Synchronization

## Goal
Replace heavyweight glFinish with lightweight fence primitives for 2x performance improvement.

## Background
Currently using glFinish which forces CPU to wait for ALL GPU work. Fences allow waiting for specific operations only.

---

## Task 1.1: Research and Design Fence API
**Description:** Investigate OpenGL fence mechanisms and design unified API

**Subtasks:**
- [ ] Research glFenceSync/glClientWaitSync
- [ ] Study Vulkan fence model for comparison
- [ ] Design platform-agnostic fence interface
- [ ] Document fence lifecycle and edge cases

**Acceptance Criteria:**
- Design document explaining fence strategy
- API specification in fence_api_design.md
- Comparison table of platform fence mechanisms
- Decision on timeout handling strategy

**Time Estimate:** 1 day

---

## Task 1.2: Implement OpenGL Fence Primitives
**Description:** Create low-level fence operations for OpenGL backend

**Subtasks:**
- [ ] Add fence creation/deletion functions
- [ ] Implement fence wait with timeout
- [ ] Add fence status query
- [ ] Create fence pool to avoid allocation

**Acceptance Criteria:**
- New module: gpu_fence_primitives.f90
- All functions have error handling
- Fence pool pre-allocates 64 fences
- Zero allocations during runtime
- Unit tests pass

**Code Example:**
```fortran
type :: gpu_fence
  integer(c_intptr_t) :: handle = 0
  logical :: is_signaled = .false.
end type

interface
  function create_fence() result(fence)
    type(gpu_fence) :: fence
  end function
  
  function wait_fence(fence, timeout_ns) result(success)
    type(gpu_fence), intent(inout) :: fence
    integer(i64), intent(in) :: timeout_ns
    logical :: success
  end function
end interface
```

**Time Estimate:** 2 days

---

## Task 1.3: Update Juggler to Use Fences
**Description:** Replace glFinish calls in juggler with fence operations

**Subtasks:**
- [ ] Add fence to each juggle buffer
- [ ] Replace glFinish with fence wait
- [ ] Implement fence-based state machine
- [ ] Add timeout recovery mechanism

**Acceptance Criteria:**
- sporkle_conv2d_juggling.f90 updated
- No glFinish calls remain
- Timeout triggers buffer skip (not hang)
- Performance test shows improvement
- Backward compatibility maintained

**Test Case:**
```fortran
! Must run without hangs
call test_juggler_timeout_recovery()
! Must show 2x improvement
call benchmark_juggler_fence_vs_finish()
```

**Time Estimate:** 2 days

---

## Task 1.4: Add Fence Support to Async Executor
**Description:** Integrate fences into GPU async executor

**Subtasks:**
- [ ] Add fence array to executor state
- [ ] Update submission to return fence
- [ ] Implement fence-based completion check
- [ ] Add multi-fence wait capability

**Acceptance Criteria:**
- gpu_async_executor.f90 uses fences
- Can track 100+ in-flight operations
- Fence status polling < 100ns
- No CPU spinning on fence wait
- Benchmarks show improved throughput

**Time Estimate:** 1 day

---

## Task 1.5: Benchmark and Profile
**Description:** Prove fence implementation delivers expected gains

**Subtasks:**
- [ ] Create fence vs finish benchmark
- [ ] Profile CPU usage during wait
- [ ] Measure dispatch latency improvement
- [ ] Test on different GPU models

**Acceptance Criteria:**
- Benchmark results documented
- 2x improvement on juggler workload
- <1µs fence wait overhead
- CPU usage drops by >50%
- Results consistent across GPUs

**Deliverables:**
- benchmark_fence_performance.f90
- Performance report with graphs
- Profile data showing CPU savings

**Time Estimate:** 1 day

---

## Task 1.6: Error Handling and Recovery
**Description:** Ensure robust fence behavior under edge cases

**Subtasks:**
- [ ] Handle GPU reset scenarios
- [ ] Implement fence leak detection
- [ ] Add debug mode with validation
- [ ] Create stress tests

**Acceptance Criteria:**
- GPU reset doesn't hang application
- Fence leaks detected and reported
- Debug mode validates all fence operations
- Stress test runs 24 hours without issues
- Clear error messages for failures

**Time Estimate:** 1 day

---

## Sprint Summary

**Total Time:** 8 days (includes buffer)

**Definition of Done:**
- [ ] All glFinish calls replaced with fences
- [ ] 2x performance improvement measured
- [ ] Zero fence-related crashes in 24hr test
- [ ] Documentation complete
- [ ] Code reviewed and merged

**Success Metrics:**
- Juggler latency: 50µs → 25µs
- CPU usage during sync: 100% → 20%
- Async executor throughput: 2x improvement
- Memory overhead: <1KB for fence pool

**Next Sprint Preview:** Zero-Copy Buffer Management
- Build on fence infrastructure
- Eliminate CPU→GPU copies
- Expected additional 1.5x speedup