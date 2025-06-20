{pkgs, ...}: final: prev: {
  dwm-workspaces = final.trivialBuild {
    pname = "dwm-workspaces";
    version = "1.0";

    src = ./dwm-workspaces.el;

    recipe = pkgs.writeText "recipe" ''
      (dwm-workspaces :fetcher github :repo "doesnt/exist" :files (:defaults "dwm-workspaces.el"))
    '';
  };
  emsg-blame = final.trivialBuild {
    pname = "emsg-blame";
    version = "1.0.0";

    packageRequires = [
      final.async
    ];

    src = pkgs.fetchFromGitHub {
      owner = "ISouthRain";
      repo = "emsg-blame";
      rev = "4cbe0584b788dee27dea796fb25fb245bdbedc68";
      hash = "sha256-FcknxrMPzWyq1DTxPK/IPne5iZyJiBsIY22PFnziwCM=";
    };
    recipe = pkgs.writeText "recipe" ''
      (emsg-blame :fetcher github :repo "ISouthRain/emsg-blame" :files (:defaults "emsg-blame.el"))
    '';
  };
  gdshader-mode = final.trivialBuild {
    pname = "gdshader-mode";
    version = "1.0.0";

    packageRequires = [
      final.glsl-mode
    ];

    src = pkgs.fetchFromGitHub {
      owner = "bbbscarter";
      repo = "gdshader-mode";
      rev = "55aac0cbb47c3a1ede39103d949946f1a6c10ace";
      hash = "sha256-Ll8QksMeLnVEkUeEOv8UryYCzXEs5kzPk+WJGBhIPA4=";
    };
    recipe = pkgs.writeText "recipe" ''
      (gdshader-mode :type git :host github :repo "bbbscarter/gdshader-mode" :files (:defaults "emsg-blame.el"))
    '';
  };
  ultra-scroll = final.trivialBuild {
    pname = "ultra-scroll";
    version = "1.0.0";

    src = pkgs.fetchFromGitHub {
      owner = "jdtsmith";
      repo = "ultra-scroll";
      rev = "78ab497c6568e4a99f34a84b4c9bfe87d1a71d8c";
      hash = "sha256-q/LGP69GRtEEbSpXi9JUoZjr/UV1QMVjQw96M6qxsZU=";
    };
    recipe = pkgs.writeText "recipe" ''
      (ultra-scroll :type git :host github :repo "jdtsmith/ultra-mode" :files (:defaults "ultra-scroll.el"))
    '';
  };
  empv = prev.empv.overrideAttrs {
    packageRequires = with final; [
      hydra
      s
    ];
  };
  rustic = prev.rustic.overrideAttrs {
    packageRequires = with final; [
      flycheck
      dash
      markdown-mode
      s
      xterm-color
      f
      rust-mode
      spinner
    ];
  };
  # "empv" = final.trivialBuild {
  #   pname = "empv.el";
  #   version = "1.0.0";

  #   packageRequires = [
  #     final.s
  #     final.consult
  #     final.compat
  #   ];

  #   src = pkgs.fetchFromGitHub {
  #     owner = "isamert";
  #     repo = "empv.el";
  #     rev = "3673dae751417a66f63a2decc8dfbcbe2712e5fc";
  #     hash = "sha256-mQMDi4MryowhzTRCWFlynqD5x0DiUWG9ITwrhZtHlVQ=";
  #   };
  #   recipe = pkgs.writeText "recipe" ''
  #     (empv :type git :host github :repo "isamert/empv.el" :files (:defaults "empv.el"))
  #   '';
  # };
  eglot-booster = final.trivialBuild {
    pname = "eglot-booster";
    version = "1.0.0";

    src = pkgs.fetchgit {
      url = "https://github.com/jdtsmith/eglot-booster";
      rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
      hash = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
    };
    recipe = pkgs.writeText "recipe" ''
      (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster" :files (:defaults "eglot-booster.el"))
    '';
  };
}
