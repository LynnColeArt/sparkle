# PM4 Debug Notes - IB Not Executing

## Current Status
- ✅ 4KB IB mapping works (no EINVAL)
- ✅ CS submission succeeds
- ✅ Fence signals
- ✅ Ring shows activity (counter changes)
- ❌ Our WRITE_DATA packet never appears in ring
- ❌ Memory doesn't change

## Ring Analysis
The ring contains:
- IT_ACQUIRE_MEM (0x22)
- IT_CONTEXT_CONTROL (0x28) 
- IT_INDIRECT_BUFFER? (0x3F)
- IT_RELEASE_MEM? (0x49)
- Lots of 0xffff1000 padding

But NO sign of our 0xC0003704 WRITE_DATA packet.

## Hypothesis
The kernel is processing our submission but not executing our IB. The CP never fetches from 0x800004000.

## For Mini
Why doesn't the CP fetch our IB even though submission succeeds?
EOF < /dev/null
