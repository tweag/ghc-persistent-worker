{lib, ...}: let

  buckCompilers = ["mwb-26-04" "mwb-26-04-fixed" "profiled-fixed"];
in {

  options = {

    buckCompilers = lib.mkOption {
      description = "Which of our branches to use for the buck build";
      type = lib.types.listOf lib.types.str;
      default = buckCompilers;
    };

  };

}
