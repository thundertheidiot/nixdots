{
  lib,
  mlib,
  config,
  options,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) str listOf attrs submodule;

  inherit (lib) genAttrs;
  inherit (lib) mkOption;
  inherit (lib.attrsets) mergeAttrsList;
in {
  options = {
    meow.user = mkOpt str "thunder" {
      description = "Username";
    };

    # TODO come back to this
    # meow.userSettings = mkOption {
    #   type = options.users.users.type.nestedTypes.elemType;
    # };
  };

  # config = {
  #   # TODO replace the inherit name part
  #   users.users = genAttrs config.meow.users (name:
  #     config.meow.userSettings
  #     // {
  #       inherit name;
  #     });
  # };
}
