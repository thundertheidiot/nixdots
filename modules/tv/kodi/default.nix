{
  kodi-wayland,
  callPackage,
  ...
}:
kodi-wayland.withPackages (kodiPkgs:
    with kodiPkgs; [
      youtube
      jellyfin
      # (kodi-wayland.packages.toKodiAddon (callPackage ./script.firefox.launcher {}))
      (kodi-wayland.packages.buildKodiAddon {
        pname = "firefox-launcher";
        version = "1.0.0";
        namespace = "script.firefox.launcher";

        src = ./script.firefox.launcher;
      })
    ])
