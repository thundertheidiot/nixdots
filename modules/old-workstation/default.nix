let
  modules = [
    (import ./utils/generic.nix)
    (import ./utils/kde.nix)
    (import ./programs/alacritty.nix)
    (import ./programs/gnome-keyring.nix)
    (import ./environment/default.nix)
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
      # TODO useless?
      environment.systemPackages = with pkgs; [
        distrobox
      ];

      # TODO: move all this shit
      security.polkit.enable = true;

      # boot.kernelPackages = pkgs.linuxPackages_cachyos-lto;

      services.cpupower-gui.enable = true;

      xdg.portal = {
        enable = true;
        xdgOpenUsePortal = true;

        config.common.default = "";
      };

      services.flatpak.enable = true;

      # TODO: move sddm
      services.displayManager.sddm = lib.mkMerge [
        (lib.mkIf (config.meow.gpu != "none") {
          enable = true;
          package = lib.mkForce pkgs.kdePackages.sddm;
          wayland = {
            enable = true;
            # compositor = "kwin";
          };
        })
        # TODO nvidia
      ];
    };
  };

  home = {mlib, ...}: {
    imports = mlib.getHomes modules;
  };
}
