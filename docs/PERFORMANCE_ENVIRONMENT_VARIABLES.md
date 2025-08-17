# Sparkle Performance Environment Variables

This document lists all environment variables that affect Sparkle's performance.

## CPU Performance

### `OMP_NUM_THREADS`
- **Purpose**: Set the number of OpenMP threads for CPU parallelization
- **Default**: System dependent (usually number of cores)
- **Recommended**: Set to number of physical cores (e.g., `16` for AMD Ryzen 7900X)
- **Example**: `export OMP_NUM_THREADS=16`
- **Impact**: Can improve CPU performance from ~1 GFLOPS to 15-25 GFLOPS

### `SPARKLE_MAX_CPU_THREADS`
- **Purpose**: Limit maximum CPU threads Sparkle will use
- **Default**: All available threads
- **Example**: `export SPARKLE_MAX_CPU_THREADS=8`
- **Use case**: Reserve CPU resources for other processes

### `SPARKLE_THREAD_RESERVE`
- **Purpose**: Reserve threads for system use (subtracted from max threads)
- **Default**: 0
- **Example**: `export SPARKLE_THREAD_RESERVE=2`
- **Use case**: Ensure system responsiveness during heavy computation

## GPU Performance

### `SPARKLE_GPU_ASYNC`
- **Purpose**: Control GPU async executor (6.5x performance improvement)
- **Default**: `1` (enabled as of latest update)
- **Values**: 
  - `1`, `on`, `true`, `yes` - Enable async executor (3,630 GFLOPS)
  - `0`, `off`, `false`, `no` - Disable async executor (451 GFLOPS)
- **Example**: `export SPARKLE_GPU_ASYNC=0` (to disable)
- **Impact**: Enables triple-buffered async GPU execution for 6.5x speedup

### `SPARKLE_GPU_DYNAMIC_SHADERS`
- **Purpose**: Enable dynamic shader generation for workload-specific optimization
- **Default**: `0` (disabled)
- **Values**: `0` or `1`
- **Example**: `export SPARKLE_GPU_DYNAMIC_SHADERS=1`
- **Status**: Implemented but not connected to production

## Device Selection

### `SPARKLE_DEVICE_HINT`
- **Purpose**: Hint for device selection
- **Default**: `auto`
- **Values**: `cpu`, `gpu`, `auto`
- **Example**: `export SPARKLE_DEVICE_HINT=gpu`
- **Note**: Currently only manual hints work; auto selection is not fully implemented

## Debugging and Profiling

### `SPARKLE_PROFILE`
- **Purpose**: Enable performance profiling output
- **Default**: `1` (enabled)
- **Values**: `0` or `1`
- **Example**: `export SPARKLE_PROFILE=0` (to disable)

### `SPARKLE_DEBUG`
- **Purpose**: Enable debug output
- **Default**: `0` (disabled)
- **Values**: `0` or `1`
- **Example**: `export SPARKLE_DEBUG=1`

## Performance Tips

### For Maximum CPU Performance (250+ GFLOPS target):
```bash
export OMP_NUM_THREADS=16  # Or number of physical cores
export SPARKLE_MAX_CPU_THREADS=16
```

### For Maximum GPU Performance (3,630 GFLOPS):
```bash
# Async is now enabled by default!
# To disable (not recommended):
export SPARKLE_GPU_ASYNC=0
```

### For Balanced System:
```bash
export OMP_NUM_THREADS=12  # Leave some for GPU driver
export SPARKLE_THREAD_RESERVE=4
```

## Known Issues

1. **SIMD CPU optimization (196.7 GFLOPS)** - Implemented but not fully integrated
2. **Dynamic shader generation** - Implemented but not connected to production
3. **Intelligent device selection** - Only manual hints work, auto selection broken

## Legacy/Deprecated

- `SPORKLE_GPU_ASYNC` - Typo version, now fixed to `SPARKLE_GPU_ASYNC`