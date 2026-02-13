<div align="center">

<img src="./sandstone-logo.png" alt="A weathered block of sandstone, the project logo" width="220">

# Sandstone

### Fine-grained Haskell builds, powered by Nix dynamic derivations.

Compile each Haskell module as its own content-addressed Nix derivation, so changing one module rebuilds one module, not the world, and everyone who shares a cache shares the compiled results.

[![Haskell](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org) [![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://nixos.org) [![Obsidian](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems) [![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)

</div>

> [!WARNING]
> **Status: early prototype.** Sandstone builds the bundled example project end to end, but it relies on unreleased Nix features and a work-in-progress fork of `hnix-store`, assumes Linux (`x86_64-linux`), and still has sharp edges throughout. It is ready for experimentation and contribution, not for production builds.

## Why

Nix builds a Haskell package as a single derivation: GHC compiles the whole package at once, and the smallest source change invalidates the entire build. Cabal's incremental rebuilds soften this locally, but that work is invisible to Nix and unshared between machines and CI.

Sandstone makes the unit of caching a **module** rather than a package. Each module becomes its own derivation, so:

- changing one module rebuilds only that module and the modules that depend on it;
- independent modules compile in parallel across all your cores;
- compiled modules are content-addressed and cached, so a build can reuse modules already compiled by anyone who shares your binary cache.

## How it works

Sandstone is glue between two existing mechanisms:

- **[GHC's `-M` dependency dump](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#dependency-generation)** produces the inter-module dependency graph.
- **[Nix dynamic derivations](https://nix.dev/manual/nix/latest/development/experimental-features#xp-feature-dynamic-derivations)** let a build *produce* a derivation, so the per-module graph can be generated on the fly and then built.

The pipeline, from a source tree to a linked binary:

1. Run `ghc -M` to emit a Makefile describing each module's `.o`/`.hi` dependencies.
2. Parse that Makefile into a dependency graph (`Sandstone.GhcMakefile`).
3. Walk the graph and emit, as a dynamic derivation:
   - one **compilation derivation per module**, content-addressed, with separate `object` and `interface` outputs, each depending only on the interfaces of the modules it imports;
   - one **link derivation** that combines the object files into the final executable.
4. Hand the derivation graph to Nix, which builds and caches it.

`hs-boot` files, mutually-recursive modules, and nested module hierarchies are all handled. See [`example/`](./example) for a small project that exercises each case.

## Requirements

Sandstone needs a build of Nix with the `ca-derivations` and `dynamic-derivations` experimental features enabled, plus the `builder-rpc-v0` system feature. The repository pins a suitable Nix ([`dep/nix`](./dep/nix)) and the `hnix-store` fork it builds against ([`dep/hnix-store`](./dep/hnix-store)), so you do not have to assemble them yourself.

## Trying it out

Build the pinned Nix, then compile the bundled example project module-by-module:

```bash
# 1. Build the pinned Nix that supports dynamic derivations.
nix-build -A nix

# 2. Compile and link example/ through Sandstone, into an alternate store at /tmp/sand.
out=$(./result/bin/nix build -f . dyn-drvs-test-res \
  --store /tmp/sand \
  --substituters https://cache.nixos.org \
  --extra-experimental-features 'ca-derivations dynamic-derivations' \
  --extra-system-features builder-rpc-v0 \
  -L -v --print-out-paths)

# 3. The build used the /tmp/sand store, so prefix it to run the resulting binary.
/tmp/sand"$out"
```

`dyn-drvs-test-res` (defined in [`default.nix`](./default.nix)) runs Sandstone *inside* a Nix build under `builder-rpc-v0`: it generates the per-module derivation graph for `example/`, adds it to the store over the daemon protocol, and registers it as the build's output, which Nix then builds into a runnable executable.

## Repository layout

- [`src/Sandstone/`](./src/Sandstone): the library. Makefile parsing and the module graph (`GhcMakefile`), derivation generation (`WriteDerivation`), the daemon protocol backend (`RemoteStore`), and accumulating errors (`Error`).
- [`src-bin/`](./src-bin): two demos. `demo-ca` drives a build from the outside against a local store; `demo-dyn-drv` runs inside a Nix build and is the body of the dynamic derivation.
- [`example/`](./example): a small Haskell project used as a build fixture (nested modules and an `hs-boot` cycle included).
- [`dep/`](./dep): pinned dependencies as [nix-thunk](https://github.com/obsidiansystems/nix-thunk) thunks (`nixpkgs`, `nix`, and the `hnix-store` fork).
- [`default.nix`](./default.nix) / [`release.nix`](./release.nix): Nix entry points and the `dyn-drvs-test` wiring.

## About Obsidian Systems

Sandstone is built and maintained by **[Obsidian Systems](https://obsidian.systems)**. We provide frontier engineering for high-assurance systems: we build production software in Haskell and Nix, and we're long-time stewards of open-source tooling like [Obelisk](https://github.com/obsidiansystems/obelisk), [Reflex](https://reflex-frp.org/), and [nix-thunk](https://github.com/obsidiansystems/nix-thunk). We also contribute to Nix itself and to [hnix-store](https://github.com/haskell-nix/hnix-store), both of which Sandstone builds on.

If you're working on build systems, Nix, or Haskell and want a partner to help design, build, or ship it, we'd love to hear from you.

- Website: <https://obsidian.systems>
- Blog: <https://blog.obsidian.systems>
- GitHub: <https://github.com/obsidiansystems>

## License

Sandstone is released under the [BSD-3-Clause License](LICENSE), © 2025 Obsidian Systems LLC.
