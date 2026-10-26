{
  inputs,
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkMerge mapAttrs' nameValuePair replaceStrings;
  inherit (mlib) mkEnOpt mkOpt jqMergeFileWithValue;
  inherit (lib.types) attrsOf anything;

  cfg = config.mHome.ai.enable && config.mHome.ai.albedo.enable;

  albedo = inputs.albedo.packages."${pkgs.system}";

  albedoWebMcp = pkgs.callPackage ./web_mcp { };

  albedoHome = "${config.xdg.dataHome}/albedo";
in
{
  options.mHome.ai.albedo = {
    enable = mkEnOpt "Set up albedo";

    # extra mcp servers other modules want merged into extensions.json
    mcpServers = mkOpt (attrsOf anything) { } {
      description = "extra mcp servers to merge into extensions.json";
    };

    # extra skills other modules want placed in ~/.albedo/skills
    skills = mkOpt (attrsOf anything) { } {
      description = "skill files to place in ~/.albedo/skills, key is the relative path";
    };
  };

  config = mkIf cfg (mkMerge [
    {
      home.file = mapAttrs'
        (name: value: nameValuePair ".albedo/skills/${name}" value)
        config.mHome.ai.albedo.skills;
    }

    {
      home.packages = [
        albedo.albedo
        albedoWebMcp
      ];

      home.sessionVariables = {
        ALBEDO_HOME = albedoHome;
      };
    }

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
      # global agents file
      home.file.".agents/global.md".source = ../global.md;

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
              mcp.servers = {
                web = {
                  type = "stdio";
                  command = "${albedoWebMcp}/bin/albedo-web-mcp";
                  callTimeoutMs = 60000;
                };
              }
              // config.mHome.ai.albedo.mcpServers;
            };
          }}
        ''}
      '';
    }
  ]);
}
