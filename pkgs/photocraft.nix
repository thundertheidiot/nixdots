{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv (
  let
    version = "0.3.0";

    photocraft = fetchzip {
      url = "https://github.com/storytold/photocraft/releases/download/v${version}/photocraft-${version}-linux-x86_64.tar.gz";
      hash = "sha256-fo0hWVyeJL+YAvro1IwRb2Khz1nJOsSlp8IgID9lg+Y=";
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
