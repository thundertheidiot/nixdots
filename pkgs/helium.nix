{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (let
  version = "0.14.9.1";

  helium = fetchzip {
    url = "https://github.com/imputnet/helium-linux/releases/download/${version}/helium-${version}-x86_64_linux.tar.xz";
    hash = "sha256-W4q9kBmcSboPKNHJzXs42sU/Yth05ZlLJV5ZO3yM4kg=";
  };
in {
  pname = "helium";
  inherit version;

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

  extraInstallCommands = ''
    mkdir -p $out/share/applications
    install -m 444 -D ${helium}/helium.desktop $out/share/applications
  '';

  runScript = "${helium}/helium";
})
