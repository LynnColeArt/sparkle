# EPIC: Neo Geo Optimization - Drain the Moat

## Mission
Apply Neo Geo principles (direct hardware access, zero abstraction) to achieve 25-30% GPU efficiency, making every device useful for AI compute.

## Success Metrics
- AMD 7900 XT: From 2,000 → 10,000+ GFLOPS  
- Dispatch latency: From 50µs → <5µs
- Memory overhead: From 20% → <5%
- Code reusability: >90% across platforms

## Core Principles
1. **Trust the hardware** - Validation is overhead
2. **Memory is memory** - Same patterns work everywhere
3. **Direct is fast** - Every layer costs performance
4. **Simple is portable** - Complex abstractions break

## Epic Breakdown

### 🎮 Sprint 1: Fence-Based Synchronization (1 week)
**Goal:** Replace glFinish with lightweight fences

**Tasks:**
- [ ] Implement fence primitives for OpenGL
- [ ] Update juggler to use fences
- [ ] Add fence support to async executor
- [ ] Benchmark improvement (expect 2x)

**Success:** Juggler runs with <1µs sync overhead

### 🏃 Sprint 2: Zero-Copy Buffer Management (1 week)
**Goal:** Eliminate memory copies between CPU/GPU

**Tasks:**
- [ ] Implement persistent mapped buffers
- [ ] Create unified buffer abstraction
- [ ] Update conv2d to use zero-copy
- [ ] Add platform-specific optimizations

**Success:** Zero CPU→GPU copies for hot paths

### 🚀 Sprint 3: PM4 Direct Submission Path (2 weeks)
**Goal:** Bypass driver for compute dispatch

**Tasks:**
- [ ] Ring buffer allocation and mapping
- [ ] PM4 command building for conv2d
- [ ] Fence integration for PM4 path
- [ ] Safety fallback mechanism

**Success:** Conv2d runs at 8,000+ GFLOPS

### 📊 Sprint 4: Autotuner Optimization (1 week)
**Goal:** Make tuning instant with caching

**Tasks:**
- [ ] GPU fingerprinting system
- [ ] Persistent tuning cache
- [ ] Reduced warmup iterations
- [ ] Cross-kernel result sharing

**Success:** Autotuning takes <100ms (from 5s)

### 🔧 Sprint 5: Platform Abstraction Layer (1 week)
**Goal:** Same code runs on AMD, Intel, Apple, Mobile

**Tasks:**
- [ ] Unified kernel description format
- [ ] Platform capability detection
- [ ] Fallback path selection
- [ ] Performance portability tests

**Success:** 90% code reuse across platforms

### 🎯 Sprint 6: Production Hardening (1 week)
**Goal:** Safe, reliable, and debuggable

**Tasks:**
- [ ] Error recovery mechanisms
- [ ] Performance monitoring
- [ ] Debug mode with validation
- [ ] Documentation and examples

**Success:** Runs 24/7 without crashes

## Technical Decisions

### What We Keep
- Juggling pattern (it's good!)
- Autotuner algorithms
- Memory optimization principles
- Fortran simplicity

### What We Replace
- Driver-based dispatch → PM4 direct
- glFinish → Fences
- Dynamic allocation → Fixed buffers
- Runtime validation → Compile-time checks

### What We Add
- Ring buffer management
- ISA shader cache
- Platform capabilities DB
- Performance regression tests

## Risk Mitigation
1. **GPU hangs** → Timeout and recovery
2. **Platform differences** → Capability detection
3. **Driver updates** → Version checking
4. **Security concerns** → Sandboxed mode

## Long-term Vision
This epic enables:
- 5-year-old phones running AI agents
- Distributed training on home hardware
- 64% idle GPU utilization
- Computational democracy

## Definition of Done
- [ ] AMD 7900 XT achieves 10,000+ GFLOPS
- [ ] Code runs on 3+ GPU vendors
- [ ] Zero memory copies in hot path
- [ ] Documentation for contributors
- [ ] Benchmarks prove improvements

## The Moat We're Draining
Every optimization removes a barrier:
- Hardware moat → Works on any GPU
- Efficiency moat → 5x performance gain
- Complexity moat → Simple, readable code
- Vendor moat → Platform independent

**Let's build the junk drawer revolution! 🚀**