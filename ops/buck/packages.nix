{config, lib}: let
  pkgs = config.envs.dev.toolchain.pkgs;

  envPackages = envName: let

    projectPackages = config.envs."buck-${envName}".toolchain.packages;
    workerPackages = config.envs.${envName}.toolchain.packages;

    toolchainLibraries = import ../ghc-toolchain-libraries.nix;

    haskellPackages =
      builtins.listToAttrs (builtins.map (p: { "name" = p.pname; "value" = p; }) haskellLibraries);

    haskellLibraries =
      let
        packages = builtins.map (n: projectPackages."${n}" or null) toolchainLibraries;
        isHaskellLibrary = p: p ? isHaskellLibrary;
      in
      builtins.filter isHaskellLibrary (lib.closePropagation packages);

  in {
    haskellPackages = pkgs.stdenvNoCC.mkDerivation {
      name = "haskellPackages";
      passthru = { libs = haskellPackages; };
      dontBuild = true;
      dontUnpack = true;
      dontConfigure = true;

      installPhase = ''
        mkdir $out
        printf "%s\n" ${ pkgs.lib.strings.concatStringsSep " " (builtins.attrValues haskellPackages) } > $out/packages
      '';
    };
    ghc = projectPackages.ghc;
    inherit (workerPackages) ghc-worker ghc-proxy buck-proxy;
  };

in {

  legacyPackages.buck = lib.genAttrs config.buckCompilers envPackages;

  packages = {
    bash = config.envs.buck.toolchain.pkgs.bash-buck;
    python = pkgs.python3;
    libuuid = pkgs.libuuid.lib;

    th-exe = pkgs.writeScriptBin "th-exe" ''
    #!${pkgs.runtimeShell}
    echo 'th-exe success'
    '';
  };

}
