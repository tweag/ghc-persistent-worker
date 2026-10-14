{lib, ...}: let

  buckCompilers = ["mercury-ghc9101" "mercury-ghc9141" "ghc914-gbc"];
in {

  options = {

    buckCompilers = lib.mkOption {
      description = "Which of our branches to use for the buck build";
      type = lib.types.listOf lib.types.str;
      default = buckCompilers;
    };

  };

}
