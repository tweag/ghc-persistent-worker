{lib, ...}: {

  options = {

    buckGhc = lib.mkOption {
      description = "Which of our branches to use for the buck build";
      type = lib.types.enum ["mwb-26-01" "ghc914"];
      default = "mwb-26-01";
    };

  };

  config = {

    buckGhc = "ghc914";

  };

}
