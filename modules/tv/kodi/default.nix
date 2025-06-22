{kodi-wayland, ...}:
kodi-wayland.withPackages (kodiPkgs:
    with kodiPkgs; [
      youtube
      jellyfin
    ])
