// Direct GPU timing using memory-mapped performance counters
// This bypasses driver overhead entirely

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <time.h>

// NVIDIA GPU memory-mapped registers (if accessible)
#define NVIDIA_TIMER_BASE 0x00088000  // Timer registers on some NVIDIA GPUs

// Read TSC with fence for accurate timing
static inline uint64_t rdtsc_fenced() {
    unsigned int lo, hi;
    __asm__ __volatile__ ("mfence");
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}

// Try to access GPU timer directly via /dev/mem (requires root)
uint64_t* map_gpu_timer() {
    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        // Try nvidia device files
        fd = open("/dev/nvidia0", O_RDWR);
        if (fd < 0) {
            return NULL;
        }
    }
    
    // Try to map GPU BAR region where timers might be
    void* mapped = mmap(NULL, 4096, PROT_READ | PROT_WRITE, 
                       MAP_SHARED, fd, NVIDIA_TIMER_BASE);
    
    if (mapped == MAP_FAILED) {
        close(fd);
        return NULL;
    }
    
    return (uint64_t*)mapped;
}

// High-resolution wall clock timing
double get_wall_time() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}

// Measure GPU kernel launch overhead
void measure_launch_overhead() {
    uint64_t start, end;
    double wall_start, wall_end;
    
    printf("=== GPU Launch Overhead Measurement ===\n\n");
    
    // Measure RDTSC overhead
    start = rdtsc_fenced();
    for (int i = 0; i < 1000000; i++) {
        __asm__ __volatile__ ("nop");
    }
    end = rdtsc_fenced();
    
    printf("RDTSC overhead test:\n");
    printf("  1M NOPs: %lu cycles\n", end - start);
    printf("  Per NOP: %.2f cycles\n\n", (end - start) / 1000000.0);
    
    // Measure clock_gettime overhead
    wall_start = get_wall_time();
    for (int i = 0; i < 1000000; i++) {
        __asm__ __volatile__ ("nop");
    }
    wall_end = get_wall_time();
    
    printf("clock_gettime overhead test:\n");
    printf("  1M NOPs: %.6f seconds\n", wall_end - wall_start);
    printf("  Per NOP: %.2f ns\n\n", (wall_end - wall_start) * 1e9 / 1000000.0);
    
    // Try to access GPU timer
    uint64_t* gpu_timer = map_gpu_timer();
    if (gpu_timer) {
        printf("✅ GPU timer mapped successfully!\n");
        printf("  Timer value: 0x%016lx\n", *gpu_timer);
    } else {
        printf("❌ Could not map GPU timer (need root or different approach)\n");
    }
}

int main() {
    measure_launch_overhead();
    return 0;
}