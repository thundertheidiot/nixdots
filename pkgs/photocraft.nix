{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.5.0";

    photocraft = fetchzip {
      url = "https://github.com/storytold/photocraft/releases/download/v${version}/photocraft-${version}-linux-x86_64.tar.gz";
      hash = "sha256-CgZ3XmRbHvm9ajj77U4EMkZ1rag/wXFsBgvbGRwkM2I=";
    };
  in
  {
    pname = "photocraft";
    inherit version;
    passthru.src = photocraft;

    targetPkgs = pkgs: with pkgs; [
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
    ];

    extraInstallCommands = ''
      cp -r ${photocraft}/share $out/
    '';

    runScript = "${photocraft}/bin/photocraft";
  }
)
