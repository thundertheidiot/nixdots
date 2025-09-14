{
  config,
  lib,
  mlib,
  modulesPath,
  server,
  pkgs,
  ...
}: let
  inherit (lib) mkForce head;
in {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disko.nix
    ./jellyfin.nix
    ./secrets
    ./wireguard.nix
  ];

  config = {
    _module.args = rec {
      server.homeServer = "10.100.0.2";
    };

    system.stateVersion = "25.05";
    time.timeZone = "Europe/Helsinki";

    users.users.root.hashedPassword = "!";

    meow = {
      workstation.enable = false;
      home.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";

      ssh.key = false;
      ssh.rootKey = true;

      server = {
        publicSSH = true;
        webserver = true;

        domains = ["saatana.xyz"];
        reverseProxy = {
          "img.${config.meow.server.mainDomain}" = "http://${server.homeServer}:2283";
        };

        certificates = ["saatana.xyz"];
        xmppDomains = ["saatana.xyz"];
        coturn = true;
        mumble = true;
      };
    };

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

    mailserver.stateVersion = 3;

    services.nginx.virtualHosts."saatana.xyz" = {
      root = ./http;
    };

    networking.networkmanager.enable = false;
    networking.useDHCP = true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";
  };
}
