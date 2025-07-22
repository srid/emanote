# Verifying copilot-setup-steps.yml

The `copilot-setup-steps.yml` file can be validated using the provided validation script to ensure all setup instructions and commands are accurate.

## Quick Validation

To verify the copilot setup file works correctly:

1. **Enter the development environment:**
   ```bash
   nix develop
   ```

2. **Run the validation script:**
   ```bash
   .github/validate-copilot-setup.sh
   ```

This script will:
- ✅ Check that all required tools are available (`cabal`, `ghc`, `just`, `ghcid`, `treefmt`, `hoogle`)
- ✅ Validate all `just` commands listed in the setup file exist and are runnable
- ✅ Verify `cabal` commands work (using dry-run mode)
- ✅ Confirm the YAML file structure is valid

## Manual Verification

You can also manually verify individual components:

### Environment Setup
```bash
nix develop
which cabal && which ghc && which just
```

### Available Commands
```bash
just --list                    # List all available commands
just --dry-run run            # Verify run command exists
just --dry-run test           # Verify test command exists
just --dry-run repl           # Verify repl command exists
just --dry-run fmt            # Verify fmt command exists
just --dry-run docs           # Verify docs command exists
```

### Build System
```bash
cabal build --dry-run         # Verify project can be built
cabal test --dry-run          # Verify tests can be run
```

## Expected Output

When validation passes, you should see:
```
🎉 All validations passed! The copilot-setup-steps.yml file is accurate.
```

If any checks fail, the script will show which specific commands or tools are missing.