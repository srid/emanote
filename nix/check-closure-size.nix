# Fail the build if emanote's runtime closure grows past `maxBytes`.
#
# Exposed as `checks.<system>.closure-size` so it runs under any standard
# flake evaluator (`nix flake check`, vira, anything using devour-flake).
# The legacy 600 MB target only held with `justStaticExecutables` +
# `removeReferencesTo`, both broken on current nixpkgs
# (https://github.com/NixOS/nixpkgs/issues/318013). This is a pure guard;
# bump `maxBytes` or restore the shrink settings to actually reduce the
# closure.
{ pkgs, emanote }:
let
  # Observed baseline at migration: ~6.77 GB. Budget sits ~10% above that
  # as a regression guard.
  maxBytes = 7500 * 1000 * 1000; # 7.5 GB
  closure = pkgs.closureInfo { rootPaths = [ emanote ]; };
in
pkgs.runCommand "emanote-closure-size-check"
{
  inherit closure;
  meta.description = "Assert emanote's Nix closure stays under ${toString maxBytes} bytes";
} ''
  size=$(xargs -a $closure/store-paths du -sb | awk '{s+=$1} END {print s}')
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
