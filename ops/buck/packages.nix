{config, lib}:
 let
  env = config.envs.buck-build;
  pkgs = env.toolchain.pkgs;
  hsPkgs = env.toolchain.packages;

  workerPkgs = config.envs.${config.buckGhc}.toolchain.packages;

  toolchainLibraries = import ../ghc-toolchain-libraries.nix;

  haskellPackages =
    builtins.listToAttrs (builtins.map (p: { "name" = p.pname; "value" = p; }) haskellLibraries);

  haskellLibraries =
    let
      packages = builtins.map (n: hsPkgs."${n}" or null) toolchainLibraries;
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
  bash = config.envs.buck.toolchain.pkgs.bash-buck;
  python = pkgs.python3;
  libuuid = pkgs.libuuid.lib;
  ghc = hsPkgs.ghc;
  ghc-worker-buck = workerPkgs.ghc-worker;
  ghc-proxy-buck = workerPkgs.ghc-proxy;
  buck-proxy-buck = workerPkgs.buck-proxy;

  th-exe = pkgs.writeScriptBin "th-exe" ''
  #!${pkgs.runtimeShell}
  echo 'th-exe success'
  '';
}
