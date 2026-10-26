{
  inputs,
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkMerge replaceStrings;
  inherit (mlib) mkEnOpt jqMergeFileWithValue;

  cfg = config.mHome.ai.enable && config.mHome.ai.albedo.enable;

  albedo = inputs.albedo.packages."${pkgs.system}";

  albedoWebMcp = pkgs.callPackage ./web_mcp { };

  albedoHome = "${config.xdg.dataHome}/albedo";
in
{
  options.mHome.ai.albedo.enable = mkEnOpt "Set up albedo";

  config = mkIf cfg (mkMerge [
    {
      home.packages = [
        albedo.albedo
        albedoWebMcp
      ];

      home.sessionVariables = {
        ALBEDO_HOME = albedoHome;
      };
    }

    # the daemon as a user unit, the cli discovers it via $ALBEDO_HOME/daemon.json
    # both come from the same flake input, so their builds always match and the
    # cli never offers to replace the systemd daemon with its own
    {
      systemd.user.services.albedo = {
        Unit = {
          Description = "albedo coding agent daemon";
        };

        Service = {
          Type = "simple";
          Environment = [ "ALBEDO_HOME=${albedoHome}" ];
          ExecStart = "${albedo.daemon}/bin/albedo-daemon";
          Restart = "on-failure";
        };

        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    }

    # the web interface, always running: the browser client is baked into the
    # cli (ALBEDO_WEB_DIST) and it finds the daemon like the terminal client
    {
      systemd.user.services.albedo-web = {
        Unit = {
          Description = "albedo web interface";
          After = [ "albedo.service" ];
        };

        Service = {
          Type = "simple";
          Environment = [ "ALBEDO_HOME=${albedoHome}" ];
          ExecStart = "${albedo.albedo}/bin/albedo web --foreground --no-open --allow 10.0.0.0/8";
          Restart = "on-failure";
        };

        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    }

    {
      home.file.".albedo/global.md".source = ../global.md;

      # try to block ~/Documents/org, this is probably broken
      xdg.dataFile."albedo/hooks.json".text =
        let
          rules = pkgs.writeShellScript "albedo-rules" (
            replaceStrings [ "@HOME@" ] [ config.home.homeDirectory ] (builtins.readFile ./rules.sh)
          );
        in
        builtins.toJSON {
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

      # extensions.json is owned and rewritten by the daemon, so merge these
      # defaults into it with jq instead of managing the file: re-applied on
      # every switch, unrelated runtime settings are kept
      home.activation.albedoDefaults = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run ${pkgs.writeShellScript "albedo-defaults" ''
          ${jqMergeFileWithValue {
            jq = "${pkgs.jq}/bin/jq";
            file = "${albedoHome}/extensions.json";
            value = {
              enabled = {
                hooks = true;
                mcp = true;
              };
              # searxng search + trafilatura fetch, see ./mcp
              mcp.servers.web = {
                type = "stdio";
                command = "${albedoWebMcp}/bin/albedo-web-mcp";
                callTimeoutMs = 60000;
              };
            };
          }}
        ''}
      '';
    }
  ]);
}
