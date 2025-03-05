{
  pkgsFunc ? import ./dep/nixpkgs,
  nixpkgsArgs ? {
    localSystem = {
      system = builtins.currentSystem;
    };
  },
}:

rec {
  inherit (pkgs) lib;

  pkgs = pkgsFunc (
    {
      overlays = [
        (import ./dep/nix).overlays.default
      ];
    }
    // nixpkgsArgs
  );

  haskellPackages = pkgs.haskellPackages.override {
    overrides = lib.composeExtensions
      (import (import ./dep/hnix-store/thunk.nix + "/overlay.nix") pkgs null)
      (self: super: {
        sandstone = self.callCabal2nix
          "sandstone"
          (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./CHANGELOG.md
              ./LICENSE
              ./sandstone.cabal
              ./src
              ./src-bin
            ];
          })
          {};

        # Until https://github.com/haskell-nix/hnix-store/pull/289's tests are fixed.
        hnix-store-json = pkgs.haskell.lib.dontCheck super.hnix-store-json;
      });
  };

  # Until a version of Nix is shipped with dynamic derivations working,
  # we'll take a version from master.
  nix = pkgs.nix;
}
