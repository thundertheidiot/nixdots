{
  fetchgit,
  fetchzip,
  kodiPackages,
}: let
  addonDir = "/share/kodi/addons";
  pins = import ./npins;
  p = name: "${pins.${name}}";
in [
  (p "plugin.video.yleareena.jade")
  (p "plugin.video.youtube")
  (p "script.module.pvr.artwork")
  (p "skin.estuary.modv2")
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.embuary.info/script.embuary.info-2.0.8.zip";
    hash = "sha256-HkUAW+kYv9+3GVbeDkqAsbykwh4/b6NenOKefNBAP8U=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.kodi-six/script.module.kodi-six-0.1.3.1.zip";
    hash = "sha256-nWz5CPoE0uVsZvWjI4q6y4ZKUnraTjTXLSJ1mK4YopI=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.html5lib/script.module.html5lib-1.1.0+matrix.1.zip";
    hash = "sha256-IJqDrCmncTMtkbCBthlukXxQraCBu3uqbcBz3+BxTKk=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.requests/script.module.requests-2.31.0.zip";
    hash = "sha256-05BSD5aoN2CTnjqaSKYMb93j5nIfLvpJHyeQsK++sTw=";
  })
  "${kodiPackages.websocket}${addonDir}/script.module.websocket"
  "${kodiPackages.six}${addonDir}/script.module.six"
  "${(kodiPackages.inputstream-adaptive.overrideAttrs (prev: {
    # remove extraInstallPhase which links a nonexistent so file
    installPhase = let
      n = "inputstream.adaptive";
      version = prev.version;
    in ''
      runHook preInstall

      make install

      [[ -f $out/lib/addons/${n}/${n}.so ]] && ln -s $out/lib/addons/${n}/${n}.so $out${addonDir}/${n}/${n}.so || true
      [[ -f $out/lib/addons/${n}/${n}.so.${version} ]] && ln -s $out/lib/addons/${n}/${n}.so.${version} $out${addonDir}/${n}/${n}.so.${version} || true

      runHook postInstall
    '';
  }))}${addonDir}/inputstream.adaptive"
  "${kodiPackages.inputstreamhelper}${addonDir}/script.module.inputstreamhelper"
  "${kodiPackages.netflix}${addonDir}/plugin.video.netflix"
  "${kodiPackages.jellyfin}${addonDir}/plugin.video.jellyfin"
  "${kodiPackages.urllib3}${addonDir}/script.module.urllib3"
  "${kodiPackages.certifi}${addonDir}/script.module.certifi"
  "${kodiPackages.signals}${addonDir}/script.module.addon.signals"
  "${kodiPackages.myconnpy}${addonDir}/script.module.myconnpy"
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.simpleeval/script.module.simpleeval-0.9.10.zip";
    hash = "sha256-7bMSSYytPxGHv9ytpXm2cZi1oxzM3hSgFVOxry0/Zqg=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.unidecode/script.module.unidecode-1.3.6.zip";
    hash = "sha256-pJrEhB2I6z8+hnWsp1m7YBJO8FE5Iw6CJrIXdlOETKY=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.simplecache/script.module.simplecache-2.0.2.zip";
    hash = "sha256-xdOBIi99nspcDIKkjxcW1r/BqL8O9NxdDViTuvMtUmo=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.skinshortcuts/script.skinshortcuts-2.0.3.zip";
    hash = "sha256-XtZ42ng3mEqsN1Vi07Tryzsgk1LgVhfIUE7hW3AHVEY=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.routing/script.module.routing-0.2.3+matrix.1.zip";
    hash = "sha256-piPmY8Q3NyIeImmkYhDwmQhBiwwcV0X532xV1DogF+I=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.module.arrow/script.module.arrow-1.2.3.zip";
    hash = "sha256-Et+9FJT1dRE1dFOrAQ70HJJcfylyLsiyay9wPJcSOXs=";
  })
  (fetchzip {
    url = "http://ftp.halifax.rwth-aachen.de/xbmc/addons/nexus/script.embuary.helper/script.embuary.helper-2.0.8.zip";
    hash = "sha256-MHwDXPcXCWsUbQTjkS8NyPwuvNllagA6k/JFj8dxwtk=";
  })
  ./script.firefox.launcher
]
