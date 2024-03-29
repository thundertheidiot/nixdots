{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";

    yleareena = {
      url = "github:aajanki/plugin.video.yleareena.jade";
      flake = false;
    };
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
      addonDir = "/share/kodi/addons";
    in {
      defaultPackage = pkgs.stdenv.mkDerivation {
        installPhase = ''
          dir="$out${addonDir}"
          mkdir -p "$dir"
          cp -R "${inputs.yleareena}" "$dir/plugin.video.yleareena.jade"
        '';

        postInstallPhase = ''
          tree $out
        '';
      };
    });
}
