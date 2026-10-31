{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.20.3.1";

    helium = fetchzip {
      url = "https://github.com/imputnet/helium-linux/releases/download/${version}/helium-${version}-x86_64_linux.tar.xz";
      hash = "sha256-mwViXcpSA6YluIFTKPxhTz0Vbntmd5X4zxIFOZT8WsE=";
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

    # helium reads browser policies from /etc/chromium/policies, but the FHS
    # sandbox mounts a tmpfs over /etc, so bind it in read-only.
    extraBwrapArgs = [
      "--ro-bind-try /etc/chromium /etc/chromium"
    ];

    runScript = "${helium}/helium";
  }
)
