{lib, ...}: rec {
  inherit (builtins) hasAttr;

  # TODO: remove when workstation rework is done
  getSystem = m: m.system or {};
  getHome = m: m.home or {};
  getSystems = list: map (m: getSystem m) list;
  getHomes = list: map (m: getHome m) list;

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
