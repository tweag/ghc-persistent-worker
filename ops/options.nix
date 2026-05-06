{lib, ...}: let

  buckCompilers = ["mwb-26-01" "ghc914"];
in {

  options = {

    buckCompilers = lib.mkOption {
      description = "Which of our branches to use for the buck build";
      type = lib.types.listOf lib.types.str;
      default = buckCompilers;
    };

  };

}
