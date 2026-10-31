{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.18.0";

    photocraft = fetchzip {
      url = "https://github.com/storytold/photocraft/releases/download/v${version}/photocraft-${version}-linux-x86_64.tar.gz";
      hash = "sha256-YFDdfTf889p3WcYJnim2V0tYcY9I3j1KwcaR8zDVdEQ=";
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
