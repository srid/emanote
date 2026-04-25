# Public interface for the e2e nix shell.
#
# Consumer:
#   mod e2e 'tests/mod.just'
#   just e2e run "cd <test-dir> && <test-runner-cmd>"
#
# Caller notes:
# - `cmd` is interpolated inside single-quotes for `nix-shell --run`, so
#   callers must not pass commands containing literal single-quote
#   characters. Use double-quoted shell strings (the typical e2e shape).
# - `[no-cd]` on `run` keeps the cwd at the caller's invocation point
#   rather than this module's own directory — the cmd is the caller's,
#   expressed relative to wherever they invoked `just`.

# Run a command inside the e2e nix shell (provides Node + Playwright Chromium)
[no-cd]
run *cmd:
    nix-shell {{source_directory()}}/shell.nix --run '{{cmd}}'
