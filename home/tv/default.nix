{ config, lib, pkgs, localconfig, ... }: {
  config = lib.mkIf (localconfig.install.tv) (with config; {
    home.packages = with pkgs; [
      kodiPackages.kodi
      kodiPackages.youtube
      kodiPackages.netflix
      kodiPackages.jellyfin
      kodiPackages.invidious
    ];
  });
}
