{
  config,
  lib,
  mlib,
  modulesPath,
  ...
}: let
  inherit (lib) mkForce;
in {
  imports = [
    ./disko.nix
    ./secrets
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  config = {
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

        certificates = ["saatana.xyz"];
        xmppDomains = ["saatana.xyz"];
        # mail.domains = ["saatana.xyz"];
        coturn = true;
        mumble = true;

        # radio = true;
      };
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
