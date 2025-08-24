#!/bin/bash
# Quick check of the critical CU enable registers
echo "GRBM_GFX_INDEX = $(sudo umr -r *.*.GRBM_GFX_INDEX 2>/dev/null | grep -o '0x[0-9a-fA-F]*' || echo 'FAILED')"
echo "THREAD_MGMT_SE0 = $(sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE0 2>/dev/null | grep -o '0x[0-9a-fA-F]*' || echo 'FAILED')"
echo "THREAD_MGMT_SE1 = $(sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE1 2>/dev/null | grep -o '0x[0-9a-fA-F]*' || echo 'FAILED')"