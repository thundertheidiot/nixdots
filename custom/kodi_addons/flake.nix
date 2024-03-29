# Optimal solution: fork nixpkgs, contribute needed addons, make a pull request
# Solution 2: Figure out how to override nixpkgs to add custom kodi addons
# Solution 3: This file

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";

    # yleareena = {
    #   url = "github:aajanki/plugin.video.yleareena.jade";
    #   flake = false;
    # };
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      lib = pkgs.lib;
      addonDir = "/share/kodi/addons";
      pythonPath = with pkgs.python311Packages; makePythonPath ([ pillow pycryptodome urllib3 certifi six webencodings ]);
      kodi-with-inputstream = pkgs.kodi.withPackages (pkgs: with pkgs; [
        inputstream-adaptive
      ]);
    in {
      defaultPackage = pkgs.stdenv.mkDerivation rec {
        name = "kodi";

        srcs = [
          (pkgs.fetchgit {
            url = "https://github.com/aajanki/plugin.video.yleareena.jade";
            rev = "9b89fabf6ad1cae08d92b3309029061ca9ab66e5";
            hash = "sha256-uLWWJTX6aHsjzJneqab2VczJO+33tg8nP7fPQ/GpqZg=";
          })
          (pkgs.fetchzip {
            url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.html5lib/script.module.html5lib-1.1.0+matrix.1.zip";
            hash = "sha256-IJqDrCmncTMtkbCBthlukXxQraCBu3uqbcBz3+BxTKk=";
          })
          (pkgs.fetchzip {
            url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.requests/script.module.requests-2.31.0.zip";
            hash = "sha256-05BSD5aoN2CTnjqaSKYMb93j5nIfLvpJHyeQsK++sTw=";
          })
          "${pkgs.kodiPackages.six}${addonDir}/script.module.six"
          # "${pkgs.kodiPackages.inputstream-adaptive}${addonDir}/inputstream.adaptive"
          # "${(let
          #   bento4 = pkgs.fetchgit {
          #     url = "https://github.com/xbmc/Bento4";
          #     rev = "1.6.0-639-7-Omega";
          #     hash = "sha256-d3znV88dLMbA4oUWsTZ7vS6WHOWzN7lIHgWPkR5Aixo=";
          #   };
          # in pkgs.stdenv.mkDerivation rec {
          #   name = "kodi-inputstream-adaptive";
          #   namespace = "inputstream.adaptive";
          #   version = "20.3.13";

          #   dontStrip = true;

          #   src = pkgs.fetchgit {
          #     url = "https://github.com/xbmc/inputstream.adaptive";
          #     rev = "${version}-Nexus";
          #     hash = "sha256-xvU+DcVEaQ/1sm6o21/6N1znCtzrct0qDhMxXGFZjL4=";
          #   };

          #   cmakeFlags = [
          #     "-DOVERRIDE_PATHS=1"
          #     "-DENABLE_INTERNAL_BENTO4=ON"
          #     "-DBENTO4_URL=${bento4}"
          #   ];

          #   nativeBuildInputs = with pkgs; [ cmake gtest ];
          #   buildInputs = with pkgs; [ kodiPackages.kodi kodiPackages.kodi-platform libcec_platform expat ];

          #   extraRuntimeDependencies = with pkgs; [ glib nspr nss stdenv.cc.cc.lib ];

          #   installPhase = let n = namespace; in ''
          #     runHook preInstall
          #     make install

          #     [[ -f $out/lib/addons/${n}/${n}.so ]] && ln -s $out/lib/addons/${n}/${n}.so $out${addonDir}/${n}/${n}.so || true
          #     [[ -f $out/lib/addons/${n}/${n}.so.${version} ]] && ln -s $out/lib/addons/${n}/${n}.so.${version} $out${addonDir}/${n}/${n}.so.${version} || true

          #     ln -s $out/lib/addons/${n}/libssd_wv.so $out/${addonDir}/${n}/libssd_wv.so

          #     runHook postInstall
          #   '';
          # })}${addonDir}/inputstream.adaptive"
          "${pkgs.kodiPackages.inputstreamhelper}${addonDir}/script.module.inputstreamhelper"
          "${pkgs.kodiPackages.youtube}${addonDir}/plugin.video.youtube"
          "${pkgs.kodiPackages.netflix}${addonDir}/plugin.video.netflix"
          "${pkgs.kodiPackages.jellyfin}${addonDir}/plugin.video.jellyfin"
          # (pkgs.fetchgit {
          #   url = "https://github.com/anxdpanic/plugin.video.youtube";
          #   rev = "d9999ec0a3c280c871e3eeb717503afcc3ff9912";
          #   hash = "sha256-dL6ZJGNhPafhIUuZbBrnZ3vFooaHftOoC6+tMUlWSEo=";
          # })
          # (pkgs.fetchgit {
          #   url = "https://github.com/CastagnaIT/plugin.video.netflix";
          #   rev = "8f82ac543a4357fb6aecbf66d9492592c5662458";
          #   hash = "sha256-7UNryHDI+2qA27+wDDdfpHPl7QLJMiSC6vRVubHTYV4=";
          # })
          "${pkgs.kodiPackages.urllib3}${addonDir}/script.module.urllib3"
          "${pkgs.kodiPackages.certifi}${addonDir}/script.module.certifi"
          "${pkgs.kodiPackages.signals}${addonDir}/script.module.addon.signals"
          "${pkgs.kodiPackages.myconnpy}${addonDir}/script.module.myconnpy"
        ];

        nativeBuildInputs = with pkgs; [ xml2 makeWrapper ];

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
              cp --recursive "$srcFile"/* "$dir/$name/"
              [ -d "$dir/$name/lib" ] && addonPythonPath="$addonPythonPath:$dir/$name/lib"
              [ -d "$dir/$name/resources/lib" ] && addonPythonPath="$addonPythonPath:$dir/$name/resources/lib"
          done

          mkdir -p "$out/bin"
          makeWrapper "${kodi-with-inputstream}/bin/kodi" "$out/bin/kodi" \
           --prefix PYTHONPATH : ${pythonPath}:$addonPythonPath \
           --prefix LD_LIBRARY_PATH ":" "${lib.makeLibraryPath (with pkgs; [ glib nspr nss stdenv.cc.cc.lib ])}"
        '';

        postInstallPhase = ''
          tree $out
        '';
      };
    });
}
