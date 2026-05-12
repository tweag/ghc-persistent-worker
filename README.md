ghc-persistent-worker
=====================

Persistent worker (compiler server) implementation.
GHC persistent worker currently works with Buck2.

<img src="docs/config.png" width="400">

Building the project
====================

The project can be built with

```bash
$ nix develop --command "cabal build all"
```

Be mindful that this runs a patched GHC 9.10.1. If you use a stock GHC 9.10.1
for other projects, compilation could fail since both toolchains write to the
same Cabal store.

```bash
$ nix develop --command "cabal path --store-dir"
```

Buck
====

The flake provides a test environment for Buck with a locally Nix-built worker.
Enter the shell `buck` to use it:

```
$ nix develop .#buck
$ buck build //ops/buck-test/three-layers/project/...

```

The flake provides the module option `buckGhc` that allows you to select the compiler you would like to use.
The option definition lists the supported values, currently `["mwb" "mwb-25-10"]`.
If you want to add an entry, you will have to create new config values in `envs` and `package-sets` with your chosen
name.
You can copy an existing config and adapt it.

The tests in `ops/buck-test` can also be run as flake apps:

```bash
$ nix run .#buck-test-restore
$ nix run .#buck-tests # All tests
```

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

An HLS version patched for the MWB GHC can be run with `nix run .#hls`. You
should configure your editor to use this command as the HLS executable.

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
============

The package `ghc-server` provides two executables that allow testing the worker in server mode without a Buck build from
the CLI.
The executable `ghc-server` starts the worker's gRPC server, while `ghc-client` sends requests in a custom format:

```
$ nix run .#ghc-server -- path/to/project &
$ nix run .#ghc-client -- path/to/project unit1:metadata unit1:modules
$ nix run .#ghc-client -- path/to/project unit2 unit3:Module3
```

Units are configured with JSON files in their directory:

```
$ ls path/to/project/unit2
Module2.hs unit.json
$ cat path/to/project/unit2/unit.json
{
  "deps": ["unit1"],
  "args": ["-package", "base"]
}
```

You can run the flake app `.#test-server` to see an example build.
