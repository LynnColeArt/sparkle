---
name: Rename Project from Sparkle to Sporkle
about: Track the comprehensive renaming effort due to naming conflict
title: '[REFACTOR] Rename project from Sparkle to Sporkle'
labels: refactoring, documentation, low-priority
assignees: ''

---

## Background

We've discovered a naming conflict with SparkleAI in the community. To avoid confusion and establish our own identity, we're renaming the project to **Sporkle**.

## Why "Sporkle"?

Like a spork (spoon + fork), Sporkle is versatile and handles multiple use cases. It's a heterogeneous compute framework that adapts to whatever hardware you have - CPU, GPU, or future accelerators. The name is:
- Unique and memorable
- Reflects our multi-purpose design philosophy  
- Avoids conflicts with existing projects
- Fun to say!

## Current Status

- External communications use "Sporkle"
- Internal code still uses "sparkle_*" naming
- Documentation is mixed

## Scope of Change

This is a comprehensive rename affecting:
- **106+ files** containing "Sparkle" references
- All module names (`sparkle_*` → `sporkle_*`)
- All type names and constants
- Environment variables
- Documentation and comments
- Build system configurations

## Why This Is Low Priority

1. **No functional impact** - The rename doesn't affect functionality
2. **Active development** - We're focused on GPU kernels and benchmarks
3. **Clean break better** - Best done as one comprehensive pass
4. **Internal consistency** - Current naming is consistent within codebase

## Implementation Plan

When we do implement this:

1. **Create dedicated branch** for the rename
2. **Automated replacement** using sed/awk for bulk changes:
   ```bash
   find . -type f -name "*.f90" -exec sed -i 's/sparkle_/sporkle_/g' {} +
   find . -type f -name "*.md" -exec sed -i 's/Sparkle/Sporkle/g' {} +
   ```
3. **Manual review** for edge cases and context-sensitive replacements
4. **Update file names** where needed
5. **Comprehensive testing** to ensure nothing breaks
6. **Documentation update** including README and all guides

## Acceptance Criteria

- [ ] All module names updated from `sparkle_*` to `sporkle_*`
- [ ] All type names updated
- [ ] All constants updated (SPARKLE_* → SPORKLE_*)
- [ ] All environment variables updated
- [ ] All documentation reflects new name
- [ ] Build system uses new name
- [ ] All tests pass with new naming
- [ ] No "Sparkle" references remain (except historical notes)

## Related Documents

See `/docs/NAMING_TODO.md` for detailed list of what needs to be renamed.

## Timeline

This will be addressed after:
1. GPU kernel implementations are complete
2. Benchmarking framework is stable
3. Core functionality is proven

---

**Note**: Until this is implemented, the project is "Sporkle" externally but "Sparkle" internally. This is intentional to avoid disrupting active development.