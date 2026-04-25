# Public interface for the `tests/nix/` portable e2e bundle.
# Consumer:
#   mod e2e 'tests/nix/mod.just'
#   just e2e run "cd <test-dir> && <test-runner-cmd>"

# Run a command inside the e2e nix shell (provides Node + Playwright Chromium)
run *cmd:
    nix-shell {{source_directory()}}/shell.nix --run '{{cmd}}'
