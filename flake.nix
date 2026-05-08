{
  description = "GHC persistent worker";

  inputs = {
    hix.url = "github:tek/hix";
    hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/b2243f41e860ac85c0b446eadc6930359b294e79";
    ghc-debug = {
      url = "git+https://gitlab.haskell.org/ghc/ghc-debug";
      flake = false;
    };
    fenix = {
      url = "github:nix-community/fenix/6c51b42ac2c25328067956ff980572482786d20c";
      inputs.nixpkgs.url = "github:nixos/nixpkgs/9807714d6944a957c2e036f84b0ff8caf9930bc0";
    };
    nix.url = "github:nixos/nix/2.34.2";
  };

  outputs = inputs@{hix, ...}: hix [({config, lib, util, ...}: {
    compiler = "ghc910";
    ghcVersions = ["ghc910" "ghc914"];
    main = "ghc-worker";
    ghci.args = ["-package ghc" "-DMWB" "-DDOWNSWEEP_CACHE" "-DUNIT_INDEX"];
    hls.genCabal = false;

    compilers = {

      # Roughly the GHC used by MWB.
      mwb-26-01-ipe.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        rev = "65d1ec83348e10082f60a4ae400cbcd31f76ad05";
        hash = "sha256-mUnXDm708rVZH9wiglOUZ6bnS83Aln6ik+r2uTfDoP0=";
      };

      # More recent GHC that includes fixed module graph nodes, with all of the custom patches present in `mwb-26-01`.
      mwb-25-10-ipe.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.12.1";
        flavour = "release+split_sections+ipe";
        rev = "99d4164fcd5cbc23c1f00bf5fd2e8f710d10bf16";
        hash = "sha256-yN0jQJiVAcBNCpHUpxPzFSGgY4TAO0xpfdzQl9L5lgs=";
      };

      ghc914.nixpkgs = "ghc914";

    };

    nixpkgs = {

      ghc914.source = {
        rev = "c6d65881c5624c9cae5ea6cedef24699b0c0a4c0";
        hash = "sha256-WNGcmeOZ8Tr9dq6ztCspYbzWFswr2mPebM9LpsfGxPk=";
      };

    };

    internal.hixCli.dev = true;

  })

  (import ./ops/packages.nix)
  (import ./ops/tools.nix)
  (import ./ops/package-sets.nix inputs)

  ];

}
