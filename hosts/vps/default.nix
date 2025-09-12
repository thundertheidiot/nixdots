{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (lib) mkForce;
in {
  imports = [
    ./disko.nix
    ./secrets
  ];

  config = {
    system.stateVersion = "25.05";
    time.timeZone = "Europe/Helsinki";

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
        coturn = true;
        mumble = true;
      };
    };

    services.nginx.virtualHosts."saatana.xyz" = {
      root = ./http;
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";
  };
}
