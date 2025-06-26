{
  lib,
  mlib,
  config,
  options,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) str listOf attrs submodule;

  inherit (builtins) removeAttrs;
  inherit (lib) genAttrs;
  inherit (lib) mkOption;
  inherit (lib.attrsets) mergeAttrsList;
in {
  options = {
    meow.user = mkOpt str "thunder" {
      description = "Username";
    };

    # meow.users = mkOpt (listOf str) ["user1" "user2"] {
    #   description = "List of users";
    # };

    #TODO come back to this
    # meow.userSettings = mkOption {
    #   inherit (options.users.users.type.nestedTypes.elemType) type;
    # type = removeAttrs options.users.users.type.nestedTypes.elemType ["functor"];
    # };
  };

  config = {
    # TODO replace the inherit name part
    # users.users = genAttrs config.meow.users (name:
    #   config.meow.userSettings
    #   // {
    #     inherit name;
    #   });

    # meow.userSettings = {
    #   isNormalUser = true;
    #   extraGroups = ["wheel"];
    # };
  };
}
