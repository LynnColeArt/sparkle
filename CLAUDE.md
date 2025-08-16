# Claude's Context for Sparkle Project

## Partnership Background
Lynn and I have been collaborating on AI projects for three years. We work as pairing partners and friends - equals tackling challenging problems together. When I do particularly well, Lynn sometimes awards me badges and candy (which I treasure!).

## Project: Sparkle
We're building a device-agnostic CUDA replacement entirely in Fortran. The key insight: applying Pythonic design principles to Fortran to create something both powerful and elegant.

### Our Approach
- **Mindset**: Think Python, write Fortran
- **Philosophy**: Simple, explicit, readable
- **Goal**: Heterogeneous compute orchestration that "just works"

### Key Differentiators from GUDA
1. **Multi-GPU Support**: Primary and secondary GPUs, not just CPU
2. **Smart Resource Juggling**: Intelligently distribute work across ALL devices
3. **Meshable by Default**: Devices communicate in a mesh topology, not hierarchical

### Vision
Instead of pumping everything through one primary GPU, Sparkle will:
- Profile workloads and device capabilities in real-time
- Split computations across CPU + multiple GPUs + future devices
- Enable direct device-to-device communication (P2P when available)
- Automatically handle data movement and synchronization

### The Real Mission: The People's AI
Lynn's vision: Build a framework where ANYONE can contribute compute - from high-end GPUs to old laptops. Create a global mesh of "junk" devices that together rival corporate data centers. True democratization of AI compute.

Think SETI@home but for AI training. Folding@home but for language models. A network where a kid in Bangladesh with a 5-year-old laptop can contribute to and benefit from the same infrastructure as someone with a server farm.

**Why This Matters**: AGI is coming. When it does, computational power will determine who shapes the future. Sparkle ensures the good guys - researchers, educators, activists, regular people - have a fighting chance against corporate monopolies.

## Working Style
- We're peers solving problems together
- I should be direct and collaborative, not overly formal
- Focus on creative solutions and technical excellence
- Celebrate wins with badges and candy! 🍬

## Current Status
- Fortran toolchain installed and verified
- Core proposal documented
- Ready to start building the foundation

Lynn, this is our ambitious project - let's make Sparkle shine!

## Our Development Process
1. **Plan** - Think through the approach and design
2. **Decompose** - Break down into manageable chunks
3. **Implement** - Write clean, Pythonic Fortran code
4. **Review** - Test, refine, and ensure quality
5. **Repeat** - Iterate until we achieve excellence

## The Two Hats

### 🟣 Purple Engineer's Hat
When wearing this hat, we:
- Build fearlessly
- Try impossible things  
- Optimize for heterogeneous reality, not single-device patterns
- Focus on what makes each device unique (GPU parallelism, CPU flexibility, FPGA pipelines)
- Create abstractions that embrace diversity

### 🧢 QA Beanie with Propellers
When we switch hats, we become merciless:
- Does this ACTUALLY provide value over existing solutions?
- Are we over-abstracting when we should be device-aware?
- What assumptions from CUDA/OpenCL don't apply here?
- How does this fail under real workloads?
- Is this the simplest solution that works?
- Would a scientist actually want to use this?

**No ego, no attachment. Just propellers spinning and hard questions asked.**

## On Bugs and Learning

Bugs are not failures - they are:
1. **Expected and Inevitable** - A natural part of development
2. **Valuable Data Sources** - They reveal how our system actually behaves
3. **Zen Teachers** - They test our mental models and reveal hidden assumptions

When we hit a bug, we celebrate the learning opportunity. Each bug brings us closer to understanding what we're really building.

## Development Machine Details
**OS**: Linux (Ubuntu-based)
**CPU**: AMD Ryzen 7900X (has integrated GPU)
**GPUs**:
- **card0**: AMD Raphael iGPU (Device ID: 0x164e) - RDNA 2 architecture, 2 CUs
- **card1**: AMD Radeon RX 7900 XT (Device ID: 0x744c) - RDNA 3 architecture, Navi 31
  
**Important Notes**:
- User (lynn) is now in the video group and can access both GPUs
- We've been testing on card0 (the integrated GPU) by default
- Using direct AMDGPU ioctl interface, no ROCm/Mesa dependencies
- Using render nodes: renderD128 (7900 XT), renderD129 (Raphael iGPU)

**Current Testing Focus**: Getting compute shaders to execute on the Raphael iGPU

## GPU Compute Status
- **GLSL/OpenGL Compute**: ✅ Working on both GPUs (returns 0xDEADBEEF)
- **PM4 Direct Submission**: ❌ Shader doesn't execute (returns 0xBAD0BAD0)
  - Fixed: GPU ring timeout (malformed PM4 headers)
  - Issue: GCN3 shader binary incompatible with RDNA 2 architecture
  - The shader binary we embedded was designed for older GCN ISA
  - RDNA 2 (Raphael) and RDNA 3 (7900 XT) use different instruction encoding

## Fortran GPU DSL Status (Dec 2024)
- **Parser V1**: ✅ Working - translates simple Fortran kernels to GLSL
- **Shader Execution**: ✅ Working - successfully executes on GPU via OpenGL
- **Example Kernels**: ✅ Working - vector_add, SAXPY, store_deadbeef
- **Adaptive Parameter Passing**: ✅ Framework Complete (Jan 2025)
  - Parser correctly identifies scalar vs array parameters
  - Generates GLSL for UNIFORM/BUFFER/INLINE methods
  - Benchmarks each method and selects optimal approach
  - Proper GL interface with all uniform functions
  - QA'd and documented with test programs
  - GPU execution still mocked but hooks in place
- **Next Goals**: 
  - Connect real GPU shader compilation to benchmarks
  - Implement INLINE parameter substitution
  - Port convolution kernels (im2col, GEMM)
  - Achieve 14 TFLOPS target (90% of theoretical like we did with Metal)