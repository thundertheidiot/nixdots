{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe getExe' mkIf mkMerge;

  cfg = config.meow.program.anki;

  anki = pkgs.anki.withAddons [ pkgs.ankiAddons.anki-connect ];
  systemctl = getExe' pkgs.systemd "systemctl";

  # the gui takes over ankiconnect while open, the headless instance comes back when it closes
  launcher = pkgs.writeShellScript "anki" ''
    ${systemctl} --user stop anki-headless || true
    trap '${systemctl} --user start anki-headless || true' EXIT
    ${getExe anki} "$@"
  '';

  wrapped = pkgs.symlinkJoin {
    name = "anki-wrapped";
    paths = [ anki ];
    inherit (anki) meta;
    postBuild = ''
      rm $out/bin/anki
      ln -s ${launcher} $out/bin/anki
    '';
  };
in
{
  config = mkIf cfg (mkMerge [
    {
      environment.systemPackages = [ wrapped ];
    }
    {
      systemd.user.services.anki-headless = {
        description = "Headless anki for ankiconnect";
        wantedBy = [ "default.target" ];
        # webengine gpu process crashes on the offscreen platform
        environment = {
          QT_QPA_PLATFORM = "offscreen";
          QT_QUICK_BACKEND = "software";
          QTWEBENGINE_CHROMIUM_FLAGS = "--disable-gpu";
        };
        serviceConfig = {
          ExecStart = getExe anki;
          Restart = "on-failure";
          RestartSec = 30;
        };
        startLimitBurst = 5;
        startLimitIntervalSec = 600;
      };
    }
    {
      systemd.user.services.anki-sync = {
        description = "Sync anki over ankiconnect";
        serviceConfig.Type = "oneshot";
        # ankiconnect being down is fine, the gui or service just isn't running
        script = ''
          ${getExe pkgs.curl} -s --max-time 120 localhost:8765 -d '{"action":"sync","version":6}' || true
        '';
      };

      systemd.user.timers.anki-sync = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "2min";
          OnUnitActiveSec = "5min";
        };
      };
    }
  ]);
}
