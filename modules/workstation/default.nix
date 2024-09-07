{
  config,
  pkgs,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf mkDefault;
  inherit (builtins) listToAttrs;

  cfg = config.meow.workstation;
in {
  options = {
    meow.workstation.enable = mkEnOpt "Configuration for workstations";
  };

  imports = [
    ./audio.nix
    ./network.nix
    ./media.nix
  ];

  config = mkIf cfg.enable {
    meow.workstation = listToAttrs (map (n: {
        name = n;
        value = mkDefault true;
      }) [
        "audio"
        "network"
        "media"
      ]);

    environment.systemPackages = with pkgs; [
      pulsemixer
      rustup
      sops

      nh

      atool
      zip
      unzip
      p7zip
      rar
      unrar
    ];

    services.libinput.enable = true;
    console = {
      useXkbConfig = true;
    };

    networking.networkmanager.enable = true;

    meow.home.modules = [
      ({config, ...}: {
        programs.gpg = {
          enable = true;
          homedir = "${config.xdg.dataHome}/gnupg";
        };

        services.gpg-agent.enable = true;

        xdg.enable = true;
      })
    ];
  };
}
