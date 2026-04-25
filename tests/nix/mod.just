# Public interface for the `tests/nix/` portable e2e bundle.
# Consumer:
#   mod e2e 'tests/nix/mod.just'
#   just e2e run "cd <test-dir> && <test-runner-cmd>"

# Run a command inside the e2e nix shell (provides Node + Playwright Chromium).
# `cmd` is interpolated inside single-quotes for `nix-shell --run`, so callers
# must not pass commands containing literal single-quote characters. Use
# double-quoted shell strings instead (the typical e2e command shape).
# `[no-cd]` keeps the cwd at the caller's invocation point rather than this
# module's own directory — the cmd is the caller's, expressed relative to
# wherever they invoked `just`.
[no-cd]
run *cmd:
    nix-shell {{source_directory()}}/shell.nix --run '{{cmd}}'
