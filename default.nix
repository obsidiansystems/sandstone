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
        # Until bump to 25.05
        (self: super: {
          nixDependencies2 = super.nixDependencies2.overrideScope (super': self': {
            boost = (super.boost.override {
                extraB2Args = [
                  "--with-container"
                  "--with-context"
                  "--with-coroutine"
                  "--with-iostreams"
                  "--with-regex"
                ];
                enableIcu = false;
              }).overrideAttrs
                (old: {
                  # Need to remove `--with-*` to use `--with-libraries=...`
                  buildPhase = lib.replaceStrings [ "--without-python" ] [ "" ] old.buildPhase;
                  installPhase = lib.replaceStrings [ "--without-python" ] [ "" ] old.installPhase;
                });
          });
        })
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

  dyn-drvs-test = pkgs.stdenv.mkDerivation {
    name = "sandstone-dyn-drvs-on-example.drv";

    ghc = pkgs.ghc.outPath;

    nativeBuildInputs = [ pkgs.nix ];

    # TODO should use ^
    bash = "${builtins.unsafeDiscardOutputDependency pkgs.bash.drvPath}!out";
    coreutils = "${builtins.unsafeDiscardOutputDependency pkgs.coreutils.drvPath}!out";
    lndir = "${builtins.unsafeDiscardOutputDependency pkgs.xorg.lndir.drvPath}!out";

    sources = ./example;

    buildCommand = ''
      export NIX_CONFIG='extra-experimental-features = nix-command ca-derivations dynamic-derivations'
      ${haskellPackages.sandstone}/bin/demo-dyn-drv
    '';

    requiredSystemFeatures = [ "recursive-nix" ];

    __contentAddressed = true;
    outputHashMode = "text";
    outputHashAlgo = "sha256";
  };

  dyn-drvs-test-res = builtins.outputOf dyn-drvs-test.outPath "out";

  # Until a version of Nix is shipped with dynamic derivations working,
  # we'll take a version from master.
  nix = pkgs.nix;
}
