#!/bin/bash
# Kernel Compiler for RDNA2
# =========================
# Compiles OpenCL/GLSL/HIP kernels to RDNA2 machine code
# Using RGA (Radeon GPU Analyzer) or LLVM/clang

set -e

# Default values
BACKEND="rga"
ARCH="gfx1030"  # RDNA2 Navi 21
OUTPUT_DIR="out"
KERNEL_FILE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --backend)
      BACKEND="$2"
      shift 2
      ;;
    --arch)
      ARCH="$2"
      shift 2
      ;;
    --output)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    *)
      KERNEL_FILE="$1"
      shift
      ;;
  esac
done

if [ -z "$KERNEL_FILE" ]; then
  echo "Usage: $0 [--backend rga|clang] [--arch gfx103x] [--output dir] kernel.cl"
  exit 1
fi

echo "Compiling kernel: $KERNEL_FILE"
echo "Backend: $BACKEND"
echo "Architecture: $ARCH"
echo "Output: $OUTPUT_DIR"

mkdir -p "$OUTPUT_DIR"

if [ "$BACKEND" = "rga" ]; then
  # RGA path - fastest for offline compilation
  echo "Using RGA (Radeon GPU Analyzer)..."
  
  # Detect kernel type
  EXT="${KERNEL_FILE##*.}"
  case $EXT in
    cl)
      SOURCE_TYPE="cl"
      ;;
    glsl|comp)
      SOURCE_TYPE="glsl"
      ;;
    hlsl)
      SOURCE_TYPE="hlsl"
      ;;
    *)
      echo "Unknown file type: $EXT"
      exit 1
      ;;
  esac
  
  # Run RGA
  rga -s "$SOURCE_TYPE" -c "$ARCH" -o "$OUTPUT_DIR" \
      --isa "$OUTPUT_DIR/ISA.txt" \
      --binary "$OUTPUT_DIR/kernel.hsaco" \
      --livereg "$OUTPUT_DIR/live.txt" \
      --metadata "$OUTPUT_DIR/meta.json" \
      "$KERNEL_FILE"
      
  echo "RGA compilation complete!"
  echo "Files generated:"
  echo "  - $OUTPUT_DIR/kernel.hsaco (binary)"
  echo "  - $OUTPUT_DIR/meta.json (resource usage)"
  echo "  - $OUTPUT_DIR/ISA.txt (disassembly)"
  
elif [ "$BACKEND" = "clang" ]; then
  # LLVM/clang path - open source toolchain
  echo "Using LLVM/clang..."
  
  # Compile to HSACO
  clang --target=amdgcn-amd-amdhsa -mcpu="$ARCH" \
        -mcode-object-version=5 \
        -nogpulib \
        -O3 -x cl -cl-std=CL2.0 \
        -c "$KERNEL_FILE" \
        -o "$OUTPUT_DIR/kernel.hsaco"
        
  # Extract metadata
  llvm-readobj --notes --sections "$OUTPUT_DIR/kernel.hsaco" > "$OUTPUT_DIR/hsaco_notes.txt"
  
  # Also disassemble
  llvm-objdump -d "$OUTPUT_DIR/kernel.hsaco" > "$OUTPUT_DIR/ISA.txt"
  
  echo "Clang compilation complete!"
  echo "Files generated:"
  echo "  - $OUTPUT_DIR/kernel.hsaco (binary)"
  echo "  - $OUTPUT_DIR/hsaco_notes.txt (metadata)"
  echo "  - $OUTPUT_DIR/ISA.txt (disassembly)"
  
else
  echo "Unknown backend: $BACKEND"
  exit 1
fi

echo ""
echo "Next steps:"
echo "1. Parse metadata to get VGPR/SGPR/LDS requirements"
echo "2. Load kernel.hsaco .text section to GPU"
echo "3. Set COMPUTE_PGM_RSRC registers based on metadata"
echo "4. Dispatch!"