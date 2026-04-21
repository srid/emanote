# Fail the build if emanote's runtime closure grows past `maxBytes`.
#
# Exposed as `checks.<system>.closure-size` so it runs under any standard
# flake evaluator — `nix flake check`, vira, anything using devour-flake —
# without an external runner. The older scaffolding also shipped a set of
# `justStaticExecutables` / `removeReferencesTo` settings; those are
# deliberately *not* restored because they broke on recent nixpkgs
# (see https://github.com/NixOS/nixpkgs/issues/318013). This is a pure
# guard — bump `maxBytes` or add shrink settings back if you ever want to
# actually reduce the closure.
{ pkgs, emanote }:
let
  # Observed baseline at migration: ~6.77 GB. The legacy 600 MB target
  # only ever held with `justStaticExecutables` + `removeReferencesTo`,
  # both broken on current nixpkgs. Until those knobs work again, this
  # budget sits ~10% above baseline purely as a regression guard.
  maxBytes = 7500 * 1000 * 1000; # 7.5 GB
in
pkgs.runCommand "emanote-closure-size-check"
{
  # `exportReferencesGraph` materialises the full runtime closure of
  # `emanote` in the sandbox and writes the reference graph to `graph`.
  # Each record is: path / deriver / nRefs / (ref × nRefs).
  exportReferencesGraph = [ "graph" emanote ];
  meta.description = "Assert emanote's Nix closure stays under ${toString maxBytes} bytes";
} ''
  paths=$(awk '
    BEGIN { state = 0 }
    state == 0 { print $0; state = 1; next }                    # path
    state == 1 { state = 2; next }                              # deriver
    state == 2 { refs = $0 + 0; state = (refs == 0 ? 0 : 3); next }  # nRefs
    state == 3 { refs -= 1; if (refs == 0) state = 0; next }    # each ref
  ' graph | sort -u)
  size=$(printf '%s\n' $paths | xargs du -sb | awk '{s+=$1} END {print s}')
  max=${toString maxBytes}
  echo "emanote closure size: $size bytes"
  echo "            max size: $max bytes"
  if [ "$size" -gt "$max" ]; then
    echo "FAIL: emanote's closure exceeds the budget."
    exit 1
  fi
  echo "OK: within budget."
  touch $out
''
