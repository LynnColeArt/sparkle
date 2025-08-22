#!/bin/bash
# Debug script to check HQD and wave status with UMR

echo "=== Checking GPU instances ==="
umr --list-instances

echo -e "\n=== Checking waves on card1 (Raphael iGPU) ==="
umr -i 1 --waves

echo -e "\n=== Checking compute ring status ==="
umr -i 1 --ring-read gfx_0.0.0 | head -20

echo -e "\n=== Checking HQD registers ==="
# HQD (Hardware Queue Descriptor) registers for compute
umr -i 1 --read mmCP_HQD_ACTIVE
umr -i 1 --read mmCP_HQD_VMID
umr -i 1 --read mmCP_HQD_PIPE_PRIORITY
umr -i 1 --read mmCP_HQD_QUEUE_PRIORITY

echo -e "\n=== Checking MEC status ==="
umr -i 1 --read mmCP_MEC_ME1_PIPE0_INT_STATUS
umr -i 1 --read mmCP_MEC_ME1_PIPE1_INT_STATUS

echo -e "\n=== Checking compute dispatch registers ==="
umr -i 1 --read mmCOMPUTE_PGM_LO
umr -i 1 --read mmCOMPUTE_PGM_HI
umr -i 1 --read mmCOMPUTE_NUM_THREAD_X
umr -i 1 --read mmCOMPUTE_NUM_THREAD_Y
umr -i 1 --read mmCOMPUTE_NUM_THREAD_Z