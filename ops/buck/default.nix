{hix, fenix, nix, ...}:
{config, lib, util, ...}: let

  globalConfig = config;

  testNames = lib.attrNames (lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../buck-test));

  testOutputNames = map (name: "buck-test-${name}") testNames;

in {

  nixpkgs.buck = {

    source = {
      rev = "9807714d6944a957c2e036f84b0ff8caf9930bc0";
      hash = "sha256-LwWRsENAZJKUdD3SpLluwDmdXY9F45ZEgCb0X+xgOL0=";
    };

    overlays = [
      fenix.overlays.default
      (import ./overlay.nix)
    ];

  };

  # The environment for the CLI tool `buck`, using the Buck overlay extracted from MWB.
  # `fenix` is a dep of Buck.
  # Exposes a devShell named `buck` that should be used to gain access to the CLI tool.
  envs.buck = {
    package-set.compiler.nixpkgs = "buck";
    package-set.compiler.source = "ghc910";
    expose.shell = true;
    packages = [];
    buildInputs = pkgs: [pkgs.buck2-source nix.packages.${pkgs.system}.default];
  };

  # The environment for our Buck nixpkgs integration, from which GHC and the package set are taken when exposing them
  # in `outputs.packages` below.
  # Uses our custom GHC build and injects a hook into all Haskell derivations that creates `package.cache` in the
  # store dir, which is needed because Buck supplies individual package DBs to GHC.
  envs.buck-build = {config, ...}: {
    packages = [];
    package-set.extends = globalConfig.buckGhc;

    overrides = api@{override, ...}: let
      testDeps = import ./test-deps.nix { inherit util; };
    in testDeps.overrides api // {
      __all = override (drv: {
        postInstall = (drv.postInstall or "") + ''
          ghc-pkg recache --package-db $packageConfDir
        '';
      });
    };
  };

  # The interface that Buck expects when loading Nix packages in `toolchains/BUCK` using those `nix.rules.flake`
  # rules.
  # Exposes the toolchain Haskell packages listed in `./ghc-toolchain-libraries.nix` in the attribute
  # `haskellPackages.libs` as well as Python and the GHC compiler derivation.
  outputs.packages =
    import ./packages.nix { inherit config lib; };

  commands = let

    buck-test = import ../buck-test/default.nix { inherit hix util; };

    buckTest = name: lib.nameValuePair "buck-test-${name}" {
      expose = true;
      env = "buck";
      command = buck-test name (import ../buck-test/${name}/default.nix { inherit util; });
    };

  in lib.listToAttrs (map buckTest testNames) // {
    buck-tests = {
      expose = true;
      env = "buck";
      command = util.hixScript "buck-tests" {} ''
      ${util.unlines (map (name: "nix run .#${name}") testOutputNames)}
      '';
    };
  };

  outputs.legacyPackages.ci-matrix = testOutputNames;

}
