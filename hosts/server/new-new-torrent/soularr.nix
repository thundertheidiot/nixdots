{pkgs, ...}: let
  slskd-api = pkgs.callPackage ({python312Packages}:
    python312Packages.buildPythonApplication rec {
      pname = "slskd-api";
      version = "0.1.5";
      format = "setuptools";

      build-system = with python312Packages; [
        setuptools
        setuptools-git-versioning
      ];

      dependencies = with python312Packages; [
        requests
      ];

      pythonImportsCheck = ["slskd_api"];

      src = pkgs.fetchPypi {
        inherit pname version;
        hash = "sha256-LmWP7bnK5IVid255qS2NGOmyKzGpUl3xsO5vi5uJI88=";
      };
    }) {};

  package = pkgs.callPackage ({
    stdenvNoCC,
    fetchFromGitHub,
    python312Packages,
    makeWrapper,
    ...
  }:
    stdenvNoCC.mkDerivation {
      name = "soularr";
      version = "1";

      src = fetchFromGitHub {
        owner = "mrusse";
        repo = "soularr";
        rev = "8e6f159e82dc05a91bb6522db0ac98f1b9ba0a02";
        hash = "sha256-/6uSCM87yVZYzzvEq0qx/ZoO6hgTja09joweLsVLq/0=";
      };

      doCheck = false;

      nativeBuildInputs = [makeWrapper];
      buildInputs = [
        python312Packages.python
      ];

      buildPhase = "";
      installPhase = ''
        mkdir -p $out/bin
        cp soularr.py $out/bin/soularr.py
        chmod +x $out/bin/soularr.py

        makeWrapper "$out/bin/soularr.py" "$out/bin/soularr" \
          --prefix PYTHONPATH : "${
          python312Packages.makePythonPath [python312Packages.music-tag python312Packages.pyarr]
        }:${slskd-api}/lib/python3.12/site-packages"
      '';
      # For some reason my slskd_api package doesn't work makePythonPath, i have no idea why
    }) {};
in {
  meow.impermanence.ensureDirectories = [
    "/persist/media_stack_data/soularr"
  ];

  systemd.timers.soularr = {
    wantedBy = ["timers.target"];
    requisite = ["slskd.service" "lidarr.service"];
    timerConfig = {
      OnBootSec = "2min";
      OnUnitActiveSec = "2min";
      Unit = "soularr.service";
    };
  };

  systemd.services.soularr = {
    vpnConfinement = {
      # access on localhost
      enable = true;
      vpnNamespace = "airvpn";
    };

    requires = ["slskd.service" "lidarr.service"];

    serviceConfig = {
      Type = "simple";
      WorkingDirectory = "/persist/media_stack_data/soularr";
      ExecStart = "${package}/bin/soularr";
    };
  };
}
