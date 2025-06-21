{
  config,
  pkgs,
  mlib,
  lib,
  inputs,
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
    ./flatpak.nix
    ./keyring.nix
    ./theme
    ./environment # de's and wm's
  ];

  config = mkIf cfg.enable {
    meow.workstation = listToAttrs (map (n: {
        name = n;
        value = {enable = mkDefault true;};
      }) [
        "audio"
        "network"
        "media"
        "theming"
        "flatpak"
        "gnomeKeyring"
      ]);

    security.polkit.enable = true;

    users.users."${config.meow.user}" = {
      extraGroups = ["wheel" "networkmanager" "docker"];
      isNormalUser = true;
    };

    nix.settings = {
      allowed-users = [config.meow.user];
      trusted-users = [config.meow.user];
    };

    boot.kernelPackages = pkgs.linuxPackages_cachyos;

    environment.systemPackages = with pkgs; [
      pulsemixer
      rustup
      sops

      nh

      ffmpeg

      atool
      zip
      unzip
      p7zip
      rar
      unrar

      yt-dlp
      # python3

      libnotify

      inputs.deploy-rs.packages."${pkgs.system}".default
    ];

    # services.cpupower-gui.enable = true;

    programs.appimage = {
      enable = true;
      binfmt = true;
    };

    services.libinput.enable = true;
    console = {
      useXkbConfig = true;
    };

    networking.networkmanager.enable = true;
  };
}
