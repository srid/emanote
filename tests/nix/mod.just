# Public interface for the `tests/nix/` portable e2e bundle.
# Consumer:
#   mod e2e 'tests/nix/mod.just'
#   just e2e run "cd <test-dir> && <test-runner-cmd>"

# Run a command inside the e2e nix shell (provides Node + Playwright Chromium).
# `cmd` is interpolated inside single-quotes for `nix-shell --run`, so callers
# must not pass commands containing literal single-quote characters. Use
# double-quoted shell strings instead (the typical e2e command shape).
run *cmd:
    nix-shell {{source_directory()}}/shell.nix --run '{{cmd}}'
