#include <stdint.h>

// Read Time Stamp Counter - gives CPU cycle count
// This bypasses all OS overhead
uint64_t rdtsc_wrapper() {
    unsigned int lo, hi;
    // RDTSC instruction - reads CPU timestamp counter
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}

// Alternative: RDTSCP for serializing version (more accurate)
uint64_t rdtscp_wrapper() {
    unsigned int lo, hi, aux;
    // RDTSCP - serializing version, ensures previous instructions complete
    __asm__ __volatile__ ("rdtscp" : "=a" (lo), "=d" (hi), "=c" (aux));
    return ((uint64_t)hi << 32) | lo;
}

// Memory fence + RDTSC for most accurate timing
uint64_t rdtsc_fenced() {
    unsigned int lo, hi;
    // Memory fence to ensure all previous memory ops complete
    __asm__ __volatile__ ("mfence");
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}