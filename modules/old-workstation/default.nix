let
  modules = [
    (import ./utils/generic.nix)
    (import ./utils/kde.nix)
    (import ./programs/alacritty.nix)
    (import ./programs/gnome-keyring.nix)
    (import ./environment/default.nix)
    (import ./theming.nix)
    (import ./laptop.nix)
  ];
in {
  system = {
    config,
    pkgs,
    lib,
    mlib,
    ...
  }: {
    imports =
      mlib.getSystems modules;

    config = lib.mkIf (config.workstation.enable) {
      environment.systemPackages = with pkgs; [
        distrobox
      ];

      security.polkit.enable = true;

      # boot.kernelPackages = pkgs.linuxPackages_cachyos-lto;

      services.cpupower-gui.enable = true;

      xdg.portal = {
        enable = true;
        xdgOpenUsePortal = true;

        config.common.default = "";
      };

      services.flatpak.enable = true;

      services.displayManager.sddm = lib.mkMerge [
        (lib.mkIf (config.meow.gpu != "none") {
          enable = true;
          wayland = {
            enable = true;
            compositor = "kwin";
          };
        })
        # TODO nvidia
      ];
    };
  };

  home = {
    config,
    pkgs,
    lib,
    mlib,
    ...
  }: {
    imports = mlib.getHomes modules;

    config =
      lib.mkIf config.workstation.enable
      {
        home.packages = with pkgs; [
          mpc-cli
          libnotify

          yle-dl
          yt-dlp
          python3

          # ansel

          qmk
        ];

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
      };
  };
}
