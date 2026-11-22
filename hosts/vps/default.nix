{
  infra,
  lib,
  modulesPath,
  ...
}:
let
  inherit (lib) mkForce;
in
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    # ./meowdzbot.nix
    ./disko.nix
    ./dns.nix
    ./freshrss.nix
    ./gatus.nix
    ./jellyfin.nix
    ./kissasome.nix
    ./monitoring
    ./navidrome.nix
    ./nya.nix
    ./secrets
    ./sodexobot.nix
    ./static.nix
    # ./urlwatch.nix
    ./website.nix
    ./wireguard.nix
  ];

  config = {
    networking.hostName = "vps";

    system.stateVersion = "25.05";
    time.timeZone = "Europe/Helsinki";

    users.users.root.hashedPassword = "!";
    hardware.enableRedistributableFirmware = false;

    meow = {
      workstation.enable = false;
      home.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";

      ssh.key = false;
      ssh.rootKey = true;

      gpu = "none";

      comin.enable = true;
      kanidm.enable = true;

      dns.enable = true;

      server = {
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
        "${infra.hosts.vps.public.v4}/32"
        "${infra.hosts.vps.public.v6}/64"
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
