{
  config,
  pkgs,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib.types) listOf attrsOf str;
  inherit (lib) mkIf;

  cfg = config.meow.tv.kodiSettings;
in {
  options.meow.tv.kodiSettings = {
    enable = mkEnOpt "Create kodi settings";
    addons =
      mkOpt (listOf str) {} {
      };
    settings =
      mkOpt (attrsOf (pkgs.formats.json {}).type) {} {
      };
  };

  config = mkIf cfg.enable {
    systemd.services."kodi-init" = {
      path = with pkgs; [
        xorg.xvfb
        procps
      ];

      wantedBy = ["multi-user.target"];
      before = ["cage-tty1.service"];

      serviceConfig = let
        kodi = pkgs.callPackage ../kodi {
          # we need x11 kodi for this
          kodi-wayland = pkgs.kodi;
        };
      in {
        Type = "oneshot";
        User = "cage";
        ExecStart = pkgs.writers.writeBash "kodi-init" ''
          if [ -d "$HOME/.kodi" ]; then
            exit 0
          fi

          Xvfb :99 -screen 0 1920x1080x24 &
          export DISPLAY=:99
          ${kodi}/bin/kodi --standalone &

          sleep 2

          pkill kodi
        '';
      };
    };

    systemd.services."kodi-addons" = {
      wantedBy = ["multi-user.target"];
      after = ["kodi-init.service"];
      requires = ["kodi-init.service"];

      serviceConfig = {
        Type = "oneshot";
        User = "cage";
        ExecStart = "${pkgs.callPackage ./addons.nix {
          kodiHome = "$HOME/.kodi";
          kodiAddons = cfg.addons;
        }}/bin/enable_addons";
      };
    };

    systemd.services."kodi-settings" = {
      wantedBy = ["multi-user.target"];
      after = ["kodi-init.service" "kodi-addons.service"];
      requires = ["kodi-init.service" "kodi-addons.service"];
      before = ["cage-tty1.service"];

      serviceConfig = {
        Type = "oneshot";
        User = "cage";
        ExecStart = "${pkgs.callPackage ./settings.nix {
          kodiHome = "$HOME/.kodi";
          kodiSettings = cfg.settings;
        }}/bin/create_settings";
      };
    };
  };
}
