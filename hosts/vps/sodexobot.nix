{
  config,
  pkgs,
  ...
}: {
  meow.impermanence.directories = [
    {path = "/var/lib/sodexobot";}
  ];

  systemd.services."sodexobot" = {
    enable = true;
    description = "Sodexobot";
    unitConfig = {
      Type = "simple";
    };
    serviceConfig = {
      ExecStart = "${pkgs.sodexobot}/bin/sodexobot";
      EnvironmentFile = config.sops.secrets."sodexobot_env".path;
      WorkingDirectory = "/var/lib/sodexobot";
      StateDirectory = "sodexobot";
    };
    wantedBy = ["multi-user.target"];
  };
}
