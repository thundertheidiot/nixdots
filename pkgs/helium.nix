{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.18.2.1";

    helium = fetchzip {
      url = "https://github.com/imputnet/helium-linux/releases/download/${version}/helium-${version}-x86_64_linux.tar.xz";
      hash = "sha256-QizbNVsqd21aCDulbtBccbYkK6jrvTpA6uphLWZ8YAg=";
    };
  in
  {
    pname = "helium";
    inherit version;
    passthru.src = helium;

    targetPkgs =
      pkgs: with pkgs; [
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
  }
)
