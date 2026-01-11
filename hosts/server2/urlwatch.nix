{
  config,
  pkgs,
  ...
}: {
  meow.impermanence.directories = [
    {path = "/var/lib/notifier";}
  ];

  users.users.urlwatch.isSystemUser = true;
  users.users.urlwatch.group = "urlwatch";
  users.groups.urlwatch = {};

  sops.secrets.urlwatch_urls.owner = "urlwatch";
  sops.secrets.urlwatch_config.owner = "urlwatch";

  systemd.services."urlwatch" = {
    enable = true;
    description = "Notify of website changes";
    environment = {
      PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
      PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
      PLAYWRIGHT_HOST_PLATFORM_OVERRIDE = "ubuntu-24.04";
    };
    serviceConfig = {
      Type = "oneshot";
      User = "urlwatch";
      WorkingDirectory = "/var/lib/urlwatch";
      StateDirectory = "urlwatch";
    };
    path = with pkgs; [urlwatch];
    script = ''
      urlwatch --cache cache.db --urls ${config.sops.secrets.urlwatch_urls.path} --config ${config.sops.secrets.urlwatch_config.path}
    '';
  };

  systemd.timers.urlwatch = {
    wantedBy = ["timers.target"];
    partOf = ["urlwatch.service"];
    timerConfig = {
      OnCalendar = "*:0/15";
      Persistent = true;
      Unit = "urlwatch.service";
    };
  };
}
