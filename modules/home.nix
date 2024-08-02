{
  lib,
  pkgs,
  config,
  mlib,
  mpkgs,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf raw attrs str;
  inherit (lib.options) literalExpression;

  cfg = config.meow.home;
in {
  options = {
    meow.home = {
      extraSpecialArgs = mkOpt attrs {inherit mlib mpkgs;} {
        example = literalExpression "{ inherit inputs; }";
      };

      sharedModules = mkOpt (listOf raw) [] {};
      modules = mkOpt (listOf raw) [] {};

      user = mkOpt str "thunder" {};
      stateVersion = mkOpt str "thunder" {};
      directory = mkOpt str "/home/${cfg.user}" {};

      file = mkOpt attrs {} { description = "Files to place in $HOME"; };
      configFile = mkOpt attrs {} { description = "Files to place in $XDG_CONFIG_HOME"; };
      dataFile = mkOpt attrs {} { description = "Files to place in $XDG_DATA_HOME"; };
    };
  };
  config = {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      backupFileExtension = "hm_backup";

      extraSpecialArgs = cfg.extraSpecialArgs;

      sharedModules = cfg.sharedModules;

      users.${cfg.user} = {
        home.stateVersion = cfg.stateVersion;
        imports = cfg.modules;

        home.file = cfg.file;
        xdg.configFile = cfg.configFile;
        xdg.dataFile = cfg.dataFile;
      };
    };
  };
}
