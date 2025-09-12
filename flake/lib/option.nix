{lib, ...}: rec {
  mkOpt = type: default: {
    example ? "",
    description ? "",
  }:
    lib.mkOption {
      inherit type default;
      example = lib.mkIf (example != "") example;
      description = lib.mkIf (description != "") example;
    };

  mkEnOpt = desc: lib.mkEnableOption desc;
  mkEnOptTrue = desc:
    lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = desc;
    };
}
