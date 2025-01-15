{
  appimageTools,
  fetchurl,
}: let
  pname = "bambustudio";
  version = "01.10.01.50"; # stupid versioning system

  src = fetchurl {
    url = "https://github.com/bambulab/BambuStudio/releases/download/v${version}/Bambu_Studio_linux_ubuntu_24.04_v01.10.01.50.AppImage";
    hash = "sha256-NH7SwQzg5Dmbn+5E2ov7EuVp4+kxgTebBRBM71ntxD8=";
  };
in
  appimageTools.wrapType2 {
    inherit pname version src;

    extraPkgs = pkgs:
      with pkgs; [
        webkitgtk_4_1
      ];
  }
