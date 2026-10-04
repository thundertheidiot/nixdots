{
  fetchzip,
  kodi,
}:
let
  pins = import ./npins;
  # Keep every addon in the package set of the Kodi variant being wrapped.
  packages = kodi.packages // {
    inputstream-adaptive = kodi.packages.inputstream-adaptive.override {
      # The locked builder tries to link a nonexistent AArch64 CDM loader.
      # Retain its normal installation and addon-library symlinks.
      buildKodiBinaryAddon =
        attrs:
        kodi.packages.buildKodiBinaryAddon (
          attrs
          // {
            extraInstallPhase = "";
          }
        );
    };
  };
  inherit (packages) buildKodiAddon;

  zipAddon =
    namespace: version: hash: attrs:
    buildKodiAddon (
      {
        pname = namespace;
        inherit namespace version;
        src = fetchzip {
          url = "https://mirrors.kodi.tv/addons/omega/${namespace}/${namespace}-${version}.zip";
          inherit hash;
        };
      }
      // attrs
    );

  webencodings =
    zipAddon "script.module.webencodings" "0.5.1+matrix.2"
      "sha256-l6vtB22EnqtBasjGXPJ+1bQ5L/4/5Bp6Ielzu/LU4cI="
      { passthru.pythonPath = "lib"; };
  html5lib =
    zipAddon "script.module.html5lib" "1.1.0+matrix.1"
      "sha256-IJqDrCmncTMtkbCBthlukXxQraCBu3uqbcBz3+BxTKk="
      {
        propagatedBuildInputs = [
          packages.six
          webencodings
        ];
        passthru.pythonPath = "lib";
      };
  simpleeval =
    zipAddon "script.module.simpleeval" "0.9.10" "sha256-7bMSSYytPxGHv9ytpXm2cZi1oxzM3hSgFVOxry0/Zqg="
      { passthru.pythonPath = "lib"; };
  unidecode =
    zipAddon "script.module.unidecode" "1.3.6" "sha256-pJrEhB2I6z8+hnWsp1m7YBJO8FE5Iw6CJrIXdlOETKY="
      { passthru.pythonPath = "lib"; };
  skinshortcuts =
    zipAddon "script.skinshortcuts" "2.0.3" "sha256-XtZ42ng3mEqsN1Vi07Tryzsgk1LgVhfIUE7hW3AHVEY="
      {
        propagatedBuildInputs = [
          unidecode
          simpleeval
        ];
      };
  image-resource-select =
    zipAddon "script.image.resource.select" "3.0.2"
      "sha256-wU4bGFzBWAYeuDstoMzPFZ3je2MzKNV7K8iG1nusSVI="
      { };
  embuary-helper =
    zipAddon "script.embuary.helper" "2.0.8" "sha256-MHwDXPcXCWsUbQTjkS8NyPwuvNllagA6k/JFj8dxwtk="
      { }; # script.module.pil is bundled with Kodi; withPackages supplies Pillow.
  embuary-info = buildKodiAddon {
    pname = "embuary-info";
    namespace = "script.embuary.info";
    version = "2.0.10";
    src = pins."script.embuary.info";
    propagatedBuildInputs = with packages; [
      requests
      arrow
      simplecache
      routing
    ];
  };
  pvr-artwork = buildKodiAddon {
    pname = "pvr-artwork";
    namespace = "script.module.pvr.artwork";
    version = "2.2.8";
    src = pins."script.module.pvr.artwork";
    propagatedBuildInputs = with packages; [
      simplecache
      requests
    ];
    passthru.pythonPath = "lib";
  };
  estuary-mod = buildKodiAddon {
    pname = "estuary-modv2";
    namespace = "skin.estuary.modv2";
    version = "21.2.1+omega.2";
    src = pins."skin.estuary.modv2";
    propagatedBuildInputs = [
      skinshortcuts
      image-resource-select
      pvr-artwork
    ];
  };
  yleareena = buildKodiAddon {
    pname = "yleareena-jade";
    namespace = "plugin.video.yleareena.jade";
    version = "1.4.0";
    src = pins."plugin.video.yleareena.jade";
    propagatedBuildInputs = [
      html5lib
      packages.requests
      packages.inputstream-adaptive
    ];
  };
  youtube =
    (packages.youtube.override {
      inherit (packages) inputstream-adaptive;
    }).overrideAttrs
      {
        name = "kodi-youtube-7.0.9+beta.3";
        version = "7.0.9+beta.3";
        src = pins."plugin.video.youtube";
      };
  netflix = packages.netflix.override {
    inherit (packages) inputstream-adaptive;
  };
  firefox-launcher = buildKodiAddon {
    pname = "firefox-launcher";
    namespace = "script.firefox.launcher";
    version = "1.0.2";
    src = ./script.firefox.launcher;
  };
in
[
  yleareena
  youtube
  pvr-artwork
  estuary-mod
  embuary-info
  embuary-helper
  firefox-launcher
]
++ (with packages; [
  websocket
  six
  kodi-six
  inputstream-adaptive
  inputstreamhelper
  netflix
  jellyfin
  urllib3
  certifi
  signals
  myconnpy
  requests
  simplecache
  routing
  arrow
])
