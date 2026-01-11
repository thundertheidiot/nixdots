{
  config,
  pkgs,
  ...
}: {
  meow.impermanence.directories = [
    {path = "/var/lib/notifier";}
  ];

  systemd.services."change-notifier" = {
    enable = true;
    description = "Notify of website changes";
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      WorkingDirectory = "/var/lib/notifier";
      StateDirectory = "notifier";
    };
    path = with pkgs; [changetower notify];
    script = ''
      cat ${config.sops.secrets.notify_links.path} | ChangeTower -s | notify -pc ${config.sops.secrets.notify_provider_config.path} -p discord
    '';
  };

  systemd.timers.change_notifier = {
    wantedBy = ["timers.target"];
    partOf = ["change-notifier.service"];
    timerConfig = {
      OnCalendar = "*:0/15";
      Persistent = true;
      Unit = "change-notifier.service";
    };
  };
}
