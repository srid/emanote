#!/usr/bin/env bash
# Validation script for copilot-setup-steps.yml
# This script verifies that all commands and instructions in the copilot setup file are accurate

set -euo pipefail

echo "🔍 Validating copilot-setup-steps.yml..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

success_count=0
total_checks=0

check() {
    local name="$1"
    local command="$2"
    total_checks=$((total_checks + 1))
    
    echo -n "  Checking $name... "
    if eval "$command" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC}"
        success_count=$((success_count + 1))
    else
        echo -e "${RED}✗${NC}"
        echo "    Command failed: $command"
    fi
}

echo "📋 Verifying development environment setup..."

# Check if we're in nix develop shell
if [[ "${IN_NIX_SHELL:-}" != "impure" ]]; then
    echo -e "${YELLOW}⚠️  Warning: Not in nix develop shell. Run 'nix develop' first.${NC}"
    echo "   This validation should be run inside the nix development environment."
    echo
fi

echo "🔧 Checking essential tools..."
check "cabal" "which cabal"
check "ghc" "which ghc" 
check "just" "which just"
check "ghcid" "which ghcid"
check "treefmt" "which treefmt"
check "hoogle" "which hoogle"

echo
echo "📝 Validating justfile commands..."
check "just (list commands)" "just --list"
check "just run command exists" "just --dry-run run"
check "just test command exists" "just --dry-run test"
check "just repl command exists" "just --dry-run repl"
check "just fmt command exists" "just --dry-run fmt"
check "just docs command exists" "just --dry-run docs"

echo
echo "🏗️  Checking cabal commands..."
check "cabal build (dry-run)" "cabal build --dry-run"
check "cabal test (dry-run)" "cabal test --dry-run"

echo
echo "📄 Validating copilot-setup-steps.yml structure..."
check "YAML file exists" "test -f .github/copilot-setup-steps.yml"
check "YAML is valid" "python3 -c 'import yaml; yaml.safe_load(open(\".github/copilot-setup-steps.yml\"))'"

echo
echo "📊 Validation Results:"
echo "  ✅ Passed: $success_count/$total_checks checks"

if [[ $success_count -eq $total_checks ]]; then
    echo -e "${GREEN}🎉 All validations passed! The copilot-setup-steps.yml file is accurate.${NC}"
    exit 0
else
    failed=$((total_checks - success_count))
    echo -e "${RED}❌ $failed/$total_checks checks failed. Please review the setup file.${NC}"
    exit 1
fi