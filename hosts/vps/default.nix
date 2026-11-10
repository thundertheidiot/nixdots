{
  config,
  lib,
  mlib,
  modulesPath,
  server,
  ...
}:
let
  inherit (lib) mkForce;
in
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    # ./authentik.nix
    # ./meowdzbot.nix
    ./disko.nix
    ./dns.nix
    ./freshrss.nix
    ./gatus.nix
    ./jellyfin.nix
    ./monitoring
    ./rathole.nix
    ./secrets
    ./sodexobot.nix
    # ./urlwatch.nix
    ./website.nix
    ./wireguard.nix
  ];

  config = {
    _module.args = {
      server.homeServer = "10.100.0.2";
      server.homeServer2 = "10.100.0.3";
    };

    networking.hostName = "vps";

    system.stateVersion = "25.05";
    time.timeZone = "Europe/Helsinki";

    users.users.root.hashedPassword = "!";
    hardware.enableRedistributableFirmware = false;

    meow = {
      certificates = false;
      workstation.enable = false;
      home.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";

      ssh.key = false;
      ssh.rootKey = true;

      gpu = "none";

      server = {
        comin.enable = true;
        publicSSH = true;
        webserver = true;

        # forgejo = {
        #   enable = true;
        #   domain = "git.kotiboksi.xyz";
        # };

        domains = [
          "kotiboksi.xyz"
          "meowcloud.net"
          "gooptyland.xyz"
          "saatana.xyz"
        ];
        mainDomain = "kotiboksi.xyz";
        reverseProxy = {
          "img.${config.meow.server.mainDomain}" = "http://${server.homeServer}:2284";
          "navidrome.meowcloud.net" = "http://${server.homeServer2}:4533";
          "vw.meowcloud.net" = "http://127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}";
        };

        jellyfinDomains = [
          "jellyfin.kotiboksi.xyz"
          "jellyfin.meowcloud.net"
        ];

        matrix.enable = true;
        matrix.domain = "meowcloud.net";
        matrix.whatsapp.enable = true;

        certificates = [
          "kotiboksi.xyz"
          "meowcloud.net"
        ];
        xmppDomains = [
          "kotiboksi.xyz"
          "gooptyland.xyz"
        ];
        coturn = true;
        # mumble = true;
        radio.enable = false;
        vaultwarden = true;
      };
    };

    services.journald.settings.Journal = {
      SystemMaxUse = "500M";
      SystemKeepFree = "200M";
      MaxFileSec = "7day";
    };

    # mailserver.stateVersion = 3;

    networking.useNetworkd = true;
    systemd.network.enable = true;
    systemd.network.networks."30-wan" = {
      matchConfig.Name = "enp1s0";
      networkConfig.DHCP = "no";
      address = [
        "95.216.151.56/32"
        "2a01:4f9:c010:9973::1/64"
      ];
      routes = [
        {
          Gateway = "172.31.1.1";
          GatewayOnLink = true;
        }
        {
          Gateway = "fe80::1";
        }
      ];
    };

    networking.enableIPv6 = true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";
  };
}
