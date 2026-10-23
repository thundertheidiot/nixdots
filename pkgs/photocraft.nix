{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.14.0";

    photocraft = fetchzip {
      url = "https://github.com/storytold/photocraft/releases/download/v${version}/photocraft-${version}-linux-x86_64.tar.gz";
      hash = "sha256-bsMZJ2Dy9VxjC610ide5idQQ561Z/Kq+nn9uPN1+bpg=";
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
