# Sporkle Reorganization Progress

## Phase 1: Lean Production Core ✅

### Completed Tasks

1. **Created new directory structure** ✅
   ```
   sporkle/
   ├─ docs/adr/              ✅ Architecture decision records
   ├─ src/
   │  ├─ core/               ✅ Core types and constants
   │  ├─ pm4/                ✅ PM4 packet builders
   │  ├─ compute/            ✅ Submit API and selftest
   │  ├─ toolchain/          ✅ Created (empty)
   │  └─ cli/                ✅ CLI entry point
   ├─ tests/                 ✅ Moved all test files here
   ├─ kernels/               ✅ Shader sources
   └─ scripts/               ✅ Build tools
   ```

2. **Consolidated preamble logic** ✅
   - Created `src/pm4/preamble.c` with standard initialization
   - Extracted common patterns from test files
   - Using libdrm's working values

3. **Created selftest framework** ✅
   - `src/compute/selftest.c` with Stage A (DEADBEEF) and Stage B (SAXPY)
   - `src/cli/sporkle.c` with `pm4 --selftest` command
   - Clear success criteria for "compute online"

4. **Moved test files** ✅
   - All `test_*.f90` files moved to `tests/`
   - Cleaned up root directory

5. **Created kernel sources** ✅
   - `kernels/deadbeef.s` - Assembly test kernel
   - `kernels/saxpy.cl` - OpenCL C kernel
   - `scripts/compile_kernel.sh` - Architecture-aware compilation

## Next Steps

### Immediate Tasks
1. **Fix submit.c compilation** 🔧
   - Need to ensure all functions from pm4_submit_impl.c are in submit.c
   - Add missing includes and dependencies

2. **Build and test** 🔧
   - Run `make` in src/ directory
   - Test `sporkle pm4 --selftest`
   - Debug any issues

3. **Delete duplicate helpers** 📝
   - Remove old submit functions
   - Update all references to use new API

### PM4 Debug Priority
Based on UMR findings, we still need to fix wave execution:
- No waves launching despite fence signaling
- Investigate MEC initialization
- Check shader endianness
- Verify compute mode setup

## Key Insights from Reorganization

1. **Single submit path** prevents BO list bugs
2. **Clear selftest** defines "working" state
3. **Separated concerns** - production vs test code
4. **Architecture-aware** kernel compilation

## Status Summary
- ✅ Directory structure created
- ✅ Core files created (needs compilation fixes)
- ✅ Test files moved
- 🔧 Build system needs testing
- 📝 Old code needs cleanup