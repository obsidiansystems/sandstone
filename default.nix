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

  # Failing loudly stops Setup at the first invocation, before a second
  # way like profiling overwrites the dump.
  cabal-ghc-shim = pkgs.writeShellScriptBin "ghc" ''
    for a in "$@"; do
      if [ "$a" = --make ]; then
        for b in "$@"; do printf '%s\0' "$b"; done > ghc-args.bin
        exit 1
      fi
    done
    exec ${pkgs.ghc}/bin/ghc "$@"
  '';

  cabal-dyn-drvs-plan = builtins.derivation {
    name = "mylib-0.1-intermediates.drv";
    system = pkgs.stdenv.hostPlatform.system;

    builder = "${haskellPackages.sandstone}/bin/cabal-dyn-drv";

    PATH = "${pkgs.coreutils}/bin";

    ghc = pkgs.ghc.outPath;
    ghcShim = "${cabal-ghc-shim}/bin/ghc";

    bash = "${builtins.unsafeDiscardOutputDependency pkgs.bash.drvPath}!out";
    coreutils = "${builtins.unsafeDiscardOutputDependency pkgs.coreutils.drvPath}!out";
    lndir = "${builtins.unsafeDiscardOutputDependency pkgs.xorg.lndir.drvPath}!out";

    sources = ./example-cabal;
    intermediatesSubdir = "share/haskell/${pkgs.ghc.version}/mylib-0.1/dist";
    # Must produce the same ghc flags as the resume derivation's configure.
    planConfigureFlags =
      "--enable-shared --enable-static --enable-library-vanilla --disable-library-profiling"
      + lib.optionalString (!pkgs.stdenv.hostPlatform.isDarwin) " --enable-split-sections";

    requiredSystemFeatures = [ "builder-rpc-v0" ];

    __contentAddressed = true;
    outputHashMode = "text";
    outputHashAlgo = "sha256";
  };

  cabal-dyn-drvs-test = haskellPackages.mkDerivation {
    pname = "mylib";
    version = "0.1";
    src = ./example-cabal;
    license = lib.licenses.bsd3;
    doCheck = false;
    doHaddock = false;
    enableLibraryProfiling = false;
    previousIntermediates = builtins.outputOf cabal-dyn-drvs-plan.outPath "out";
  };

  # Until a version of Nix is shipped with dynamic derivations working,
  # we'll take a version from master.
  nix = pkgs.nix;
}
