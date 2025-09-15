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
    ./meowdzbot.nix
    ./secrets
    ./sodexobot.nix
    ./website.nix
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
        radio = true;
      };
    };

    mailserver.stateVersion = 3;

    networking.enableIPv6 = true;
    networking.interfaces.enp1s0.ipv6.addresses = [
      {
        address = "2a01:4f9:c010:9973::1";
        prefixLength = 64;
      }
    ];
    networking.defaultGateway6 = {
      address = "fe80::1";
      interface = "enp1s0";
    };
    networking.networkmanager.enable = false;
    networking.useDHCP = true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";
  };
}
