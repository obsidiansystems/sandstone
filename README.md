# Sandstone

> [!IMPORTANT]
> This is currently a work in progress.
> It is not ready to be used outside of dogfooding by developers of Sandstone itself.

Fine-grained Haskell builds with Nix's dynamic derivations.

Methodology:

- GHC's [ability to dump makefiles](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#dependency-generation)

- Nix's [dynamic derivation](https://nix.dev/manual/nix/latest/development/experimental-features#xp-feature-dynamic-derivations)
