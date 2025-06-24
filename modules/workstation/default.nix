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

    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocales = [
      "en_US.UTF-8/UTF-8"
      "fi_FI.UTF-8/UTF-8"
    ];
    i18n.extraLocaleSettings =
      {
        LC_ALL = "en_US.UTF-8";
        LC_CTYPE = "en_US.UTF-8";
      }
      // (listToAttrs (map (name: {
          inherit name;
          value = "fi_FI.UTF-8";
        }) [
          "LC_ADDRESS"
          "LC_IDENTIFICATION"
          "LC_MEASUREMENT"
          "LC_MESSAGES"
          "LC_MONETARY"
          "LC_NAME"
          "LC_NUMERIC"
          "LC_PAPER"
          "LC_TELEPHONE"
          "LC_TIME"
          "LC_COLLATE"
        ]));
  };
}
