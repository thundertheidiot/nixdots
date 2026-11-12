{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.23.1";

    photocraft = fetchzip {
      url = "https://github.com/storytold/photocraft/releases/download/v${version}/photocraft-${version}-linux-x86_64.tar.gz";
      hash = "sha256-MbU6WJ4xSqU+Y+6VKEbfPkTT33xfYq9SDhGv2eM50rg=";
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
