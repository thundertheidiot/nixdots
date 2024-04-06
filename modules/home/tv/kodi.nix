# This package provides:
# 1. $out/share/kodi/addons, this is linked to ${config.programs.kodi.datadir}/addons by home manager
# 2. $out/bin/kodi, this is a wrapper for kodi with the correct PYTHONPATH and LD_LIBRARY_PATH

{
  lib,
  pkgs,
  config,
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
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.simpleeval/script.module.simpleeval-0.9.10.zip";
        hash = "sha256-7bMSSYytPxGHv9ytpXm2cZi1oxzM3hSgFVOxry0/Zqg=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.unidecode/script.module.unidecode-1.3.6.zip";
        hash = "sha256-pJrEhB2I6z8+hnWsp1m7YBJO8FE5Iw6CJrIXdlOETKY=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.simplecache/script.module.simplecache-2.0.2.zip";
        hash = "sha256-xdOBIi99nspcDIKkjxcW1r/BqL8O9NxdDViTuvMtUmo=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.skinshortcuts/script.skinshortcuts-2.0.3.zip";
        hash = "sha256-XtZ42ng3mEqsN1Vi07Tryzsgk1LgVhfIUE7hW3AHVEY=";
      })
      (pkgs.fetchgit {
        url = "https://github.com/b-jesch/script.module.pvr.artwork";
        rev = "6bac060ea6736be1020375e3ec143f1942eb74e1";
        hash = "sha256-aE4gMhX8uE3LyH+i4UW9CqsL/quqctXcL45g4uLzLL4=";
      })
      (pkgs.fetchzip {
        url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.embuary.helper/script.embuary.helper-2.0.8.zip";
        hash = "sha256-MHwDXPcXCWsUbQTjkS8NyPwuvNllagA6k/JFj8dxwtk=";
      })
      (pkgs.fetchgit {
        url = "https://github.com/b-jesch/skin.estuary.modv2";
        rev = "25d9514ad194a1161f0289fb793f725b7d9cd536";
        hash = "sha256-N1yLI8T8PxaldQP1z8TFv+XLYsWSBhnw9wa1hcpVcZM=";
      })
      ./script.firefox.launcher
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
      makeWrapper "${pkgs.kodi}/bin/kodi" "$out/bin/kodi_with_addons" \
       --prefix PYTHONPATH : ${pythonPath}:$addonPythonPath \
       --prefix LD_LIBRARY_PATH ":" "${lib.makeLibraryPath (with pkgs; [glib nspr nss stdenv.cc.cc.lib])}:$dir/inputstream.adaptive"
    '';
  }
