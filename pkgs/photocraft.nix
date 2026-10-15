{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.9.0";

    photocraft = fetchzip {
      url = "https://github.com/storytold/photocraft/releases/download/v${version}/photocraft-${version}-linux-x86_64.tar.gz";
      hash = "sha256-ycjUc5zv+Xe62JF13dHdDtgK+DSMj3WKKhejSyu2vfo=";
    };
  in
  {
    pname = "photocraft";
    inherit version;
    passthru.src = photocraft;

    targetPkgs =
      pkgs: with pkgs; [
        glibc.bin
        glib
        gtk3
        dbus
        libxkbcommon
        libGL
        wayland
        libx11
        libxcursor
        libxrandr
        libxi
        libxcb
      ];

    extraInstallCommands = ''
      cp -r ${photocraft}/share $out/
    '';

    runScript = "${photocraft}/bin/photocraft";
  }
)
