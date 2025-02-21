{
  pkgsFunc ? import ./dep/nixpkgs,
  nixpkgsArgs ? {
    localSystem = {
      system = builtins.currentSystem;
    };
  },
}:

rec {
  pkgs = pkgsFunc (
    {
      overlays = [
        (import ./dep/nix).overlays.default
      ];
    }
    // nixpkgsArgs
  );

  # Until a version of Nix is shipped with dynamic derivations working,
  # we'll take a version from master.
  nix = pkgs.nix;
}
