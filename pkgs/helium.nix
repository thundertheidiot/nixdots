{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv rec {
  pname = "helium-browser";
  version = "0.14.9.1";

  targetPkgs = pkgs:
    with pkgs; [
      glibc.bin # binary package
      glib
      nspr
      nss
      atk
      dbus
      cups
      expat

      libxcb
      libxkbcommon
      libX11
      libXext
      libXcomposite
      libXdamage
      libXfixes
      libXrandr

      alsa-lib
      libgbm
      cairo
      pango
      udev

      mesa
      libdrm
      libglvnd
    ];

  runScript = let
    helium = fetchzip {
      url = "https://github.com/imputnet/helium-linux/releases/download/${version}/helium-${version}-x86_64_linux.tar.xz";
      hash = "sha256-W4q9kBmcSboPKNHJzXs42sU/Yth05ZlLJV5ZO3yM4kg=";
    };
  in "${helium}/helium";
}
