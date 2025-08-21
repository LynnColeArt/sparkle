#!/bin/bash
# Setup Mini's Integrity Checking Git Hooks
# =========================================

echo "🔧 Setting up Mini's integrity checking git hooks..."
echo ""

# Set git to use our hooks directory
git config core.hooksPath .githooks

echo "✅ Git hooks configured!"
echo ""
echo "Mini will now check every commit for:"
echo "  • Mixed-kind arithmetic bugs"
echo "  • Raw system_clock usage"
echo "  • Manual FLOP calculations"
echo "  • Direct iso_fortran_env usage"
echo "  • c_f_pointer portability issues"
echo "  • GFLOPS precision errors"
echo "  • Missing error handling"
echo ""
echo "To temporarily bypass checks: git commit --no-verify"
echo "To disable: git config --unset core.hooksPath"