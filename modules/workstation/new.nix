{
  config,
  pkgs,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.workstation;
in {
  options = {
    meow.workstation.enable = mkEnOpt "Configuration for workstations";
  };

  config = mkIf cfg.enable {
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

    console = {
      useXkbConfig = true;
    };

    networking.networkmanager.enable = true;

    services.libinput.enable = true;

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
