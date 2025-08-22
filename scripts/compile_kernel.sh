#!/bin/bash
# Compile kernels for detected GPU architecture

set -e

# Detect GPU architecture
detect_gpu_arch() {
    # Read device ID from first AMD GPU
    DEVICE_ID=$(lspci -nn | grep -E "VGA.*AMD|Display.*AMD" | head -1 | grep -oP '\[[\da-f]{4}:[\da-f]{4}\]' | cut -d: -f2 | tr -d ']')
    
    case "$DEVICE_ID" in
        "164e") echo "gfx1036" ;;  # Raphael iGPU
        "744c") echo "gfx1100" ;;  # Navi 31 / 7900 XT
        "73df") echo "gfx1030" ;;  # Navi 21 / 6900 XT
        *) echo "gfx1030" ;;       # Default to RDNA2
    esac
}

# Compile assembly kernel
compile_asm() {
    local input=$1
    local output=$2
    local arch=$3
    
    echo "Compiling $input for $arch..."
    
    # Use LLVM tools to assemble
    /opt/rocm/llvm/bin/clang -target amdgcn-amd-amdhsa -mcpu=$arch \
        -c $input -o ${output}.o
    
    /opt/rocm/llvm/bin/ld.lld -shared ${output}.o -o ${output}.co
    
    # Extract code object
    /opt/rocm/bin/rocm-objdump -h ${output}.co | grep .text
}

# Compile OpenCL kernel
compile_cl() {
    local input=$1
    local output=$2
    local arch=$3
    
    echo "Compiling $input for $arch..."
    
    # Use clang to compile OpenCL C
    /opt/rocm/llvm/bin/clang -cl-std=CL2.0 \
        -target amdgcn-amd-amdhsa -mcpu=$arch \
        -Xclang -finclude-default-header \
        -c $input -o ${output}.bc
    
    # Link to executable
    /opt/rocm/llvm/bin/clang -target amdgcn-amd-amdhsa -mcpu=$arch \
        ${output}.bc -o ${output}.co
}

# Main
if [ $# -lt 2 ]; then
    echo "Usage: $0 <input> <output> [arch]"
    echo "  input: .s or .cl kernel source"
    echo "  output: output binary path"
    echo "  arch: target architecture (auto-detected if not specified)"
    exit 1
fi

INPUT=$1
OUTPUT=$2
ARCH=${3:-$(detect_gpu_arch)}

echo "Target architecture: $ARCH"

case "$INPUT" in
    *.s) compile_asm "$INPUT" "$OUTPUT" "$ARCH" ;;
    *.cl) compile_cl "$INPUT" "$OUTPUT" "$ARCH" ;;
    *) echo "Unknown input type: $INPUT"; exit 1 ;;
esac

echo "Done! Output: ${OUTPUT}.co"