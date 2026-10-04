{
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOpt;

  cfg = config.mHome.opencode.enable;
in
{
  options = {
    mHome.opencode.enable = mkEnOpt "Set up opencode";
  };

  config = mkIf cfg {
    programs.opencode = {
      enable = true;

      settings = {
        permission = {
          # ~/Documents/org contains sensitive stuff
          external_directory = {
            "*" = "ask";
            "/nix/store/**" = "allow";
            "/tmp/opencode/**" = "allow";
            "~/Documents/org" = "deny";
            "~/Documents/org/**" = "deny";
          };

          read = {
            "~/Documents/org" = "deny";
            "~/Documents/org/**" = "deny";
          };

          edit = {
            "~/Documents/org" = "deny";
            "~/Documents/org/**" = "deny";
          };
        };
      };

      web = {
        enable = true;
        extraArgs = [
          "--hostname"
          "0.0.0.0"
          "--port"
          "4096"
        ];
      };
    };
  };
}
