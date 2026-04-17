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
    ghcVersions = [];
    main = "ghc-worker";
    ghci.args = ["-package ghc" "-DMWB" "-DDOWNSWEEP_CACHE" "-DUNIT_INDEX" "-DFIXED_NODES"];
    hls.genCabal = false;

    compilers = {

      # Roughly the GHC used by MWB.
      mwb-26-04.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        rev = "fe5932d418e8221bb5bcd34954caca07ff8d9310";
        hash = "sha256-4ZhFSs6ZC3XXmrcmZ72ca0sn+RZVNgx5rbiwZuRJigY=";
      };

      # Some as `mwb-26-04`, but with fixed nodes.
      mwb-26-04-fixed.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.10.1";
        flavour = "release+split_sections+ipe";
        rev = "630ee987758fe2fce24113afd445fa95e4b82503";
        hash = "sha256-frUOGkwnYbCJooJtMYVpdGEWmi05rKjP6Dd2h0zq/8Q=";
      };

      # More recent GHC that includes fixed module graph nodes, with all of the custom patches present in `mwb-26-04`.
      mwb-25-10-ipe.source.build = {
        url = "https://gitlab.haskell.org/ghc/ghc";
        version = "9.12.1";
        flavour = "release+split_sections+ipe";
        rev = "99d4164fcd5cbc23c1f00bf5fd2e8f710d10bf16";
        hash = "sha256-yN0jQJiVAcBNCpHUpxPzFSGgY4TAO0xpfdzQl9L5lgs=";
      };

    };

    # The compiler from the above set used for Buck builds (more precisely, the name of an env using some compiler).
    buckGhc = "mwb-26-04";

    internal.hixCli.dev = true;

  })

  (import ./ops/options.nix)
  (import ./ops/packages.nix)
  (import ./ops/tools.nix)
  (import ./ops/package-sets.nix inputs)
  (import ./ops/buck/default.nix inputs)

  ];

}
