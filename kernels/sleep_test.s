// sleep_test.s - Minimal sleep kernel to verify dispatch
// ======================================================
// Just sleeps to prove the GPU is executing code

        .text
        .globl sleep_test
        .p2align 8
sleep_test:
        s_sleep 100        // Sleep for 100 cycles
        s_endpgm           // End program