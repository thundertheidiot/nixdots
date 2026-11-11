{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe' makeBinPath mkAfter mkIf mkMerge replaceStrings;
  inherit (mlib) mkEnOpt;
  inherit (builtins) readFile toJSON;

  cfg = config.mHome.ai.enable && config.mHome.ai.albedo.enable;

  albedoWebMcp = pkgs.callPackage ./web_mcp { };
in
{
  options.mHome.ai.albedo.enable = mkEnOpt "Set up albedo";

  config = mkIf cfg (mkMerge [
    {
      programs.albedo.enable = true;
      services.albedo.enable = true;

      home.packages = [ albedoWebMcp ];

      services.albedo = {
        enableMcpIntegration = true;
        settings.extensions.enabled.hooks = true;
      };

      # searxng search + trafilatura fetch, see ./web_mcp
      programs.mcp.servers.web = {
        command = getExe' albedoWebMcp "albedo-web-mcp";
        callTimeoutMs = 60000;
      };
    }

    {
      # upstream path is only the hm profile, add the system ones back
      systemd.user.services.albedo.Service.Environment = mkAfter [
        "PATH=${makeBinPath [
          config.home.profileDirectory
          "/run/wrappers"
          "/run/current-system/sw"
        ]}"
      ];
    }

    {
      systemd.user.services.albedo-web = {
        Unit = {
          Description = "albedo web interface";
          After = [ "albedo.service" ];
        };

        Service = {
          Type = "simple";
          ExecStart = "${getExe' config.programs.albedo.package "albedo"} web --foreground --no-open --allow 10.0.0.0/8";
          Restart = "on-failure";
        };

        Install.WantedBy = [ "default.target" ];
      };
    }

    {
      # global agents file
      home.file.".agents/global.md".source = ../global.md;
      home.file.".agents/web.md".text = ''
        Prefer the web mcp over your other search tools.
      '';

      # try to block ~/Documents/org, this is probably broken
      home.file.".albedo/hooks.json".text =
        let
          rules = pkgs.writeShellScript "albedo-rules" (
            replaceStrings [ "@HOME@" ] [ config.home.homeDirectory ] (readFile ./rules.sh)
          );
        in
        toJSON {
          hooks.PreToolUse = [
            {
              matcher = "*";
              hooks = [
                {
                  type = "command";
                  command = "${rules}";
                  timeout = 10;
                }
              ];
            }
          ];
        };
    }
  ]);
}
