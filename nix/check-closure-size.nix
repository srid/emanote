# `checks.<system>.closure-size` regression guard. The legacy 600 MB
# target required `justStaticExecutables` + `removeReferencesTo`, both
# broken on current nixpkgs (https://github.com/NixOS/nixpkgs/issues/318013).
# Until those work again, the budget tracks the observed baseline.
{ pkgs, emanote }:
let
  maxBytes = 7500 * 1000 * 1000; # 7.5 GB; ~10% above 6.77 GB baseline
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
