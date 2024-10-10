let
  modules = [
    (import ./utils/generic.nix)
    (import ./utils/kde.nix)
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

      # boot.kernelPackages = pkgs.linuxPackages_cachyos-lto;

      services.cpupower-gui.enable = true;
    };
  };

  home = {mlib, ...}: {
    imports = mlib.getHomes modules;
  };
}
