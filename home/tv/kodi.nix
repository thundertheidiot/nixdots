# This package provides:
# 1. $out/share/kodi/addons, this is linked to $KODI_HOME/addons by home manager
# 2. $out/bin/kodi, this is a wrapper for kodi with the correct PYTHONPATH and LD_LIBRARY_PATH

{
  lib,
  pkgs,
  ...
}: let
  addonDir = "/share/kodi/addons";
  pythonPath = with pkgs.python311Packages; makePythonPath [pillow pycryptodome urllib3 certifi six webencodings chardet charset-normalizer idna six dateutil];
in
  pkgs.stdenv.mkDerivation rec {
    name = "kodi_with_addons";
    srcs = [
      (pkgs.fetchgit {
        url = "https://github.com/aajanki/plugin.video.yleareena.jade";
        rev = "9b89fabf6ad1cae08d92b3309029061ca9ab66e5";
        hash = "sha256-uLWWJTX6aHsjzJneqab2VczJO+33tg8nP7fPQ/GpqZg=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.kodi-six/script.module.kodi-six-0.1.3.1.zip";
        hash = "sha256-nWz5CPoE0uVsZvWjI4q6y4ZKUnraTjTXLSJ1mK4YopI=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.html5lib/script.module.html5lib-1.1.0+matrix.1.zip";
        hash = "sha256-IJqDrCmncTMtkbCBthlukXxQraCBu3uqbcBz3+BxTKk=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.requests/script.module.requests-2.31.0.zip";
        hash = "sha256-05BSD5aoN2CTnjqaSKYMb93j5nIfLvpJHyeQsK++sTw=";
      })
      "${pkgs.kodiPackages.websocket}${addonDir}/script.module.websocket"
      "${pkgs.kodiPackages.six}${addonDir}/script.module.six"
      "${pkgs.kodiPackages.inputstream-adaptive}${addonDir}/inputstream.adaptive"
      "${pkgs.kodiPackages.inputstreamhelper}${addonDir}/script.module.inputstreamhelper"
      "${pkgs.kodiPackages.netflix}${addonDir}/plugin.video.netflix"
      "${pkgs.kodiPackages.jellyfin}${addonDir}/plugin.video.jellyfin"
      # "${pkgs.kodiPackages.youtube}${addonDir}/plugin.video.youtube"
      (pkgs.fetchgit {
        url = "https://github.com/anxdpanic/plugin.video.youtube";
        rev = "d9999ec0a3c280c871e3eeb717503afcc3ff9912";
        hash = "sha256-dL6ZJGNhPafhIUuZbBrnZ3vFooaHftOoC6+tMUlWSEo=";
      })
      "${pkgs.kodiPackages.urllib3}${addonDir}/script.module.urllib3"
      "${pkgs.kodiPackages.certifi}${addonDir}/script.module.certifi"
      "${pkgs.kodiPackages.signals}${addonDir}/script.module.addon.signals"
      "${pkgs.kodiPackages.myconnpy}${addonDir}/script.module.myconnpy"
    ];

    nativeBuildInputs = with pkgs; [xml2 makeWrapper];

    # This needs to happen for some reason???
    unpackPhase = ''
      for srcFile in $srcs; do
        echo
      done
    '';

    installPhase = ''
      dir="$out${addonDir}"
      mkdir -p "$dir"
      addonPythonPath=""
      for srcFile in $srcs; do
          name=$("${pkgs.xml2}/bin/xml2" < "$srcFile"/addon.xml | grep '/addon/@id=' | sed 's/\/addon\/@id=//g')
          mkdir -p "$dir/$name"
          cp --dereference --recursive "$srcFile"/* "$dir/$name/"
          [ -d "$dir/$name/lib" ] && addonPythonPath="$addonPythonPath:$dir/$name/lib"
          [ -d "$dir/$name/libs" ] && addonPythonPath="$addonPythonPath:$dir/$name/libs"
          [ -d "$dir/$name/resources/lib" ] && addonPythonPath="$addonPythonPath:$dir/$name/resources/lib"
      done

      mkdir -p "$out/bin"
      makeWrapper "${pkgs.kodi}/bin/kodi" "$out/bin/kodi" \
       --prefix PYTHONPATH : ${pythonPath}:$addonPythonPath \
       --prefix LD_LIBRARY_PATH ":" "${lib.makeLibraryPath (with pkgs; [glib nspr nss stdenv.cc.cc.lib])}:$dir/inputstream.adaptive"
    '';
  }
