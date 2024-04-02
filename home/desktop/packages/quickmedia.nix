{ lib, pkgs ? import <nixpkgs> {}, ... }: let
  sibs = import ./sibs.nix { inherit pkgs;};
in pkgs.stdenv.mkDerivation rec {
  name = "quickmedia";
  src = pkgs.fetchgit {
    url = "https://repo.dec05eba.com/QuickMedia";
    rev = "714ed0e235a600502c489d08d78b3781e18fc327";
    hash = "sha256-6IVmg56f1nQg7nqZxojfNxiawUM2SIHVev4bZe9vlXs=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs; [ sibs ninja pkg-config curl gnutar makeWrapper fd gnused ];
  buildInputs = with pkgs; [ mpv xorg.libX11 xorg.libXrender xorg.libXrandr ];

  buildPhase = ''
    export HOME=./temp
    mkdir --parents $HOME

    # Lazy patch
    ${pkgs.fd}/bin/fd '.*\.cpp|.*\.hpp|.*\.h' -x ${pkgs.gnused}/bin/sed -i "s#/usr/share#$out/share#g"

    ${sibs}/bin/sibs build --release video_player
    ${sibs}/bin/sibs build --release
  '';

  installPhase = ''
    mkdir --parents "$out/bin"
    mkdir --parents "$out/share/quickmedia"
    mkdir --parents "$out/share/applications"

    # ${pkgs.curl}/bin/curl -sfL 'https://dec05eba.com/files/twemoji.tar.gz' -o "$TMP/twemoji.tar.gz"
    # ${pkgs.gnutar}/bin/tar xf "$TMP/twemoji.tar.gz" --directory=$out/share/quickmedia/emoji
    # rm -f "$TMP/twemoji.tar.gz"

    platform=$(${sibs}/bin/sibs platform)
    install -Dm755 "video_player/sibs-build/$platform/release/quickmedia-video-player" "$out/bin/quickmedia-video-player"
    install -Dm755 "sibs-build/$platform/release/quickmedia" "$out/bin/.quickmedia-unwrapped"
    install -Dm644 boards.json "$out/share/quickmedia/boards.json"

    install -Dm644 example-config.json "$out/share/quickmedia/example-config.json"
    install -Dm644 README.md "$out/share/quickmedia/README.md"

    install -Dm644 mpv/fonts/Material-Design-Iconic-Font.ttf "$out/share/quickmedia/mpv/fonts/Material-Design-Iconic-Font.ttf"
    install -Dm644 mpv/scripts/mordenx.lua "$out/share/quickmedia/mpv/scripts/mordenx.lua"
    install -Dm644 mpv/scripts/ytdl_hook.lua "$out/share/quickmedia/mpv/scripts/ytdl_hook.lua"
    install -Dm644 mpv/input.conf "$out/share/quickmedia/mpv/input.conf"
    install -Dm644 mpv/mpv.conf "$out/share/quickmedia/mpv/mpv.conf"

    
    for file in images/* icons/* shaders/* themes/*; do
      install -Dm644 "$file" "$out/share/quickmedia/$file"
    done

    for file in launcher/*; do
      filename=$(basename "$file")
      install -Dm644 "$file" "$out/share/applications/$filename"
    done

    makeWrapper "$out/bin/.quickmedia-unwrapped" "$out/bin/quickmedia" \
     --prefix PATH ":" "${pkgs.fontconfig.bin}/bin/" \
     --prefix LD_LIBRARY_PATH ":" "${lib.makeLibraryPath (with pkgs; [ libglvnd ])}"
  '';
}
