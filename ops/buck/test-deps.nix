{util}: let

  inherit (util) pkgs lib config;

  testDepWithModule = {name, modName, num, mod, extraDeps ? ""}: let

    cabalName = "${name}.cabal";

    cabal = pkgs.writeText cabalName ''
    cabal-version: 3.0
    name: ${name}
    version: 1
    build-type: Simple
    library
      hs-source-dirs: "."
      exposed-modules: ${modName}
      build-depends: base, template-haskell${extraDeps}
      default-extensions:
        TypeApplications
      default-language: GHC2021
    '';

    src = pkgs.stdenv.mkDerivation {
      name = "${name}-src";

      buildCommand = ''
      mkdir $out
      cp ${cabal} $out/${cabalName}
      cp ${mod} $out/${modName}.hs
      '';

    };

  in pkgs.haskell.lib.compose.overrideCabal (drv: {
    doCheck = false;
    doHaddock = false;
    postInstall = (drv.postInstall or "") + ''
    ghc-pkg recache --package-db $packageConfDir
    '';
  }) (config.envs.${config.buckGhc}.toolchain.packages.callCabal2nix name src {});

  modInt = {modName, bindName, expr, imports ? ""}:
  pkgs.writeText "${modName}.hs" ''
  module ${modName} where
  ${imports}
  ${bindName} :: Int
  ${bindName} = ${expr}
  '';

  testDep = {num, expq ? true, extraDeps ? ""}: let

    name = "dep${builtins.toString num}";

    modName = "Dep${builtins.toString num}";

    bindName = bnum: "${name}_${builtins.toString bnum}";

    bind = bnum: let
      bname = bindName bnum;
    in [
      "${bname} :: ExpQ"
      "${bname} = lift @_ @Int ${builtins.toString bnum}"
    ];

    binds = lib.concatMap bind (lib.range 1 100);

    mod = pkgs.writeText "${modName}.hs" ''
    module ${modName} where
    import Language.Haskell.TH (ExpQ)
    import Language.Haskell.TH.Syntax (lift)
    ${util.unlines binds}
    '';

  in testDepWithModule {
    inherit name modName mod num extraDeps;
  };

  tdep1 = {
    name = "tdep1";
    package = testDepWithModule {
      name = "tdep1";
      modName = "TDep1";
      num = 1;
      mod = modInt {
        modName = "TDep1";
        bindName = "tdep1";
        expr = "1";
        imports = "import Data.Semigroup.Generic";
      };
      extraDeps = ", semigroups";
    };
  };

  tdep2 = {
    name = "tdep2";
    package = testDepWithModule {
      name = "tdep2";
      modName = "TDep2";
      num = 2;
      mod = modInt {
        modName = "TDep2";
        bindName = "tdep2";
        expr = "2";
      };
    };
  };

  deps =
    [tdep1] ++
    map (num: { name = "dep${builtins.toString num}"; package = testDep { inherit num; }; }) (lib.range 1 100)
    ;

in {
  inherit deps;
  paths = pkgs.writeText "test-deps" (util.unlines (map ({package, ...}: package) deps));

  overrides = {drv, ...}:
  lib.listToAttrs (map ({name, package}: lib.nameValuePair name (drv package)) deps);

}
