{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOptTrue mkEnOpt;
  inherit (lib) mkDefault mkIf mkMerge;

  cfg = config.mHome.emacs;
in {
  options = {
    mHome.emacs.enable = mkEnOptTrue "Emacs";
    mHome.emacs.exwm = mkEnOpt "exwm";
  };

  config = mkIf cfg.enable {
    programs.man.generateCaches = mkDefault config.mHome.emacs.enable;

    home.packages = with pkgs;
      mkMerge [
        (mkIf cfg.exwm [
          wmctrl
        ])
        [
          emacs-lsp-booster
        ]
      ];

    xdg.mimeApps.defaultApplications = builtins.listToAttrs (builtins.map
      (mime: {
        name = mime;
        value = ["emacsclient.desktop"];
      }) [
        "text/plain"
        "application/plain"
      ]);

    xdg.configFile."emacs/.createdir" = {
      enable = true;
      text = "This file is here to make nix create \"${config.xdg.configHome}/emacs/\", so emacs uses it instead of \"${config.home.homeDirectory}/.emacs.d/\".";
    };
    services.emacs = {
      enable = true;
      defaultEditor = true;
      client.enable = true;
      startWithUserSession = "graphical";
    };

    programs.emacs = {
      enable = true;

      package = pkgs.emacsWithPackagesFromUsePackage {
        config = pkgs.replaceVars ./config.org (let
          tangle = cfg:
            if cfg
            then "yes"
            else "no";

          l = config.mHome.lang;
        in {
          lang_nix = tangle l.nix;
          lang_haskell = tangle l.haskell;
          lang_rust = tangle l.rust;
          lang_lua = tangle l.lua;
          lang_python = tangle l.python;
          # TODO make emacs setup for these
          # lang_c_cxx  = tangle l.c_cxx;
          # lang_bash = tangle l.bash;
          # lang_web = tangle l.web;

          exwm_enable = tangle cfg.exwm;
        });

        alwaysTangle = true;
        alwaysEnsure = true;
        defaultInitFile = true;

        extraEmacsPackages = epkgs:
          with epkgs; [
            treesit-grammars.with-all-grammars
          ];

        package =
          if config.mHome.emacs.exwm
          then pkgs.emacs-gtk
          else pkgs.emacs-pgtk;

        override = import ./overrides.nix {inherit pkgs;};
      };
    };
  };
}
