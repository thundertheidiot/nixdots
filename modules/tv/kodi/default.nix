{
  kodi-wayland,
  callPackage,
  ...
}:
kodi-wayland.withPackages (kodiPkgs:
    with kodiPkgs; [
      youtube
      jellyfin
      (callPackage ./script.firefox.launcher {})
    ])
