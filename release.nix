let
  lib = import (import ./dep/nixpkgs/thunk.nix + "/lib");
in

lib.genAttrs
  [
    "x86_64-linux"
    "aarch64-darwin"
  ]
  (
    system:
    let
      proj = import ./. {
        nixpkgsArgs = {
          localSystem = { inherit system; };
        };
      };
    in
    {
      inherit (proj.haskellPackages) sandstone;
      inherit (proj) nix;
    }
  )
