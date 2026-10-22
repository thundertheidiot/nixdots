{
  config,
  mlib,
  lib,
  pkgs,
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
    # install the tools as copies instead of symlinks
    # bun resolves the imports locally, there's no @opencode-ai in the nix store
    home.activation.opencodeTools = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run mkdir -p $HOME/.config/opencode/tools
      run install -m644 ${./tools}/*.ts $HOME/.config/opencode/tools/
    '';

    programs.opencode = {
      enable = true;

      enableMcpIntegration = true;
      context = ./global.md;

      extraPackages = [ pkgs.python3Packages.trafilatura ];

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
