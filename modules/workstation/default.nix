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

      yle-dl
      yt-dlp
      # python3

      libnotify

      inputs.deploy-rs.packages."${pkgs.system}".default
    ];

    services.cpupower-gui.enable = true;

    programs.appimage = {
      enable = true;
      binfmt = true;
    };

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
        xdg.userDirs = {
          enable = true;
          createDirectories = true;
          documents = "${config.home.homeDirectory}/Documents";
          download = "${config.home.homeDirectory}/Downloads";
          music = "${config.home.homeDirectory}/Music";
          pictures = "${config.home.homeDirectory}/Pictures";
          videos = "${config.home.homeDirectory}/Videos";
          desktop = "${config.home.homeDirectory}/.local/share/xdg-dirs/desktop";
          publicShare = "${config.home.homeDirectory}/.local/share/xdg-dirs/publicshare";
          templates = "${config.home.homeDirectory}/.local/share/xdg-dirs/templates";
        };
      })
    ];
  };
}
