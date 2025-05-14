ghc-persistent-worker
=====================

Persistent worker (compiler server) implementation.
GHC persistent worker currently works with Buck2.

<img src="docs/config.png" width="400">

Buck
====

The flake provides a test environment for Buck with a locally Nix-built worker.
Enter the shell `buck` to use it:

```
$ nix develop .#buck
$ buck build //ops/buck-test/three-layers/project/...

```
(optionally, one can set `--package-db (pkg_db_path)`. One can have multiple `--package-db`.)

Cabal Flags
===========

The project has a few optional features that depend on recent or experimental patches in GHC.
These can be toggled by specifying corresponding Cabal flags, which enable CPP defines during the build.
For example, running `cabal build -ffixed-nodes` enables the fixed nodes feature.

* Fixed nodes `-ffixed-nodes`/`-DFIXED_NODES`

  This feature takes advantage of the new lightweight module graph nodes to reduce the time it takes to restore module
  graphs from the file system cache.
  Fixed nodes only store the path to a module's interface, rather than requiring the parsed AST like conventional module
  graph nodes, but can not be used to compile the module.
  We use this node type when a module and its dependencies haven't been modified since the last build, so it can be
  expected that it won't be requested for compilation.

* Unit index `-funit-index`/`-DUNIT_INDEX`

  In projects with a large number of units, the performance of unit state initialization degrades substantially, because
  GHC wasn't designed for this use case.
  In particular, the package DB files of each unit's dependencies are read from disk unconditionally, even though they
  are often identical across units.
  Furthermore, the lookup table that associates module names with the packages that contain them is duplicated with
  heavy redundancies.
  In this GHC patch, these operations are abstracted away, allowing us to provide an efficient implementation locally
  that shares all of this data.

* Downsweep cache `-fdownsweep-cache`/`-DDOWNSWEEP_CACHE`

  This is a simple optimization that allows reusing old module graphs when recomputing a new graph, which we use to
  provide dependency graphs from our state.

Local GHC development
=====================

You can build the worker executable with a GHC built in a local checkout, as long as the GHC configured in `flake.nix`
is binary compatible (i.e. you made some changes to the branch used here, and didn't change the interface format).

Assuming the build directory is `/path/to/ghc/_build`, you can execute:

```
nix run .#rebuild-impure-worker /path/to/ghc/_build
```

The app will print the path of the executable, which can then be inserted into the `binary_path` attribute of the
`impure_worker` target named `impure_ghc_worker` in `toolchains/BUCK`.
In order to use it, the `persistent_worker` target must set the attribute `worker = ":impure_ghc_worker"`.

This will cause subsequent Buck builds to use the new worker executable.

HLS
===

When the GHC used to build HLS includes patches that influence CPP pragmas in the worker, you need to enable those in
`cabal.project`.

An HLS version patched for the MWB GHC can be run with `nix run .#hls`.

Cachix
======

In order to avoid having to rebuild GHC when first using a new upstream change, you can add this Cachix instance to your
Nix config:

```nix
  nix = {
    settings.substituters = ["https://tek.cachix.org"];
    settings.trusted-public-keys = ["tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk="];
  };
```

It can be provided as CLI arguments as well:

```
$ nix --option extra-substituters https://tek.cachix.org --option extra-trusted-public-keys tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk= run .#buck-tests
```

Without Buck
