{
  buildFHSEnv,
  fetchzip,
}:
buildFHSEnv rec {
  pname = "glide-browser";
  version = "0.1.66a";

  # yes, all of this is required for webgl to work
  targetPkgs =
    pkgs: with pkgs; [
      glibc.bin # binary package
      gtk3
      alsa-lib

      mesa
      libdrm
      libglvnd

      # x11 libs (needed for gpu discovery)
      libx11
      libxcb
      libXext
      libXfixes
      libXdamage
      libxxf86vm
      libxrandr
      libXcomposite
      libXcursor
      libxi

      # misc
      pango
      cairo
      atk
      gdk-pixbuf
      glib
    ];

  # Regular Glide releases use prerelease-style tags.
  passthru.autoUpdate.args = [ "--version=unstable" ];
  passthru.src = fetchzip {
    url = "https://github.com/glide-browser/glide/releases/download/${version}/glide.linux-x86_64.tar.xz";
    hash = "sha256-aafWIGS1s9QVDCT9tkCLhJnjDMxs7NS7b/dFy6K18Sc=";
  };

  runScript = "${passthru.src}/glide";
}
