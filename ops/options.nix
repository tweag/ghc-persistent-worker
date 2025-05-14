{lib, ...}: {

  options = {

    buckGhc = lib.mkOption {
      description = "Which of our branches to use for the buck build";
      type = lib.types.enum ["mwb-25-10" "mwb-26-01"];
      default = "mwb-26-01";
    };

  };
}
