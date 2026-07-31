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

  # Can't use stdenv.mkDerivation here, since builder-rpc-v0 builds don't get
  # $out in their environment and stdenv's setup bails without it.
  dyn-drvs-test = builtins.derivation {
    name = "link.drv";
    system = pkgs.stdenv.hostPlatform.system;

    builder = "${haskellPackages.sandstone}/bin/demo-dyn-drv";

    ghc = pkgs.ghc.outPath;

    # TODO should use ^
    bash = "${builtins.unsafeDiscardOutputDependency pkgs.bash.drvPath}!out";
    coreutils = "${builtins.unsafeDiscardOutputDependency pkgs.coreutils.drvPath}!out";
    lndir = "${builtins.unsafeDiscardOutputDependency pkgs.xorg.lndir.drvPath}!out";

    sources = ./example;

    requiredSystemFeatures = [ "builder-rpc-v0" ];

    __contentAddressed = true;
    outputHashMode = "text";
    outputHashAlgo = "sha256";
  };

  dyn-drvs-test-res = builtins.outputOf dyn-drvs-test.outPath "out";

  # Until a version of Nix is shipped with dynamic derivations working,
  # we'll take a version from master.
  nix = pkgs.nix;
}
