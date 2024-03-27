{ config, lib, pkgs, localconfig, ... }: {
  config = lib.mkIf (localconfig.install.tv) (with config; {
    programs.kodi = {
      enable = true;
      package = pkgs.kodi.withPackages (exts: with exts; [
        youtube
        netflix
        jellyfin
        invidious
      ]);
    };
  });
}
