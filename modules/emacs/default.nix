{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  cfg = config.meow.emacs;

  inherit (mlib) mkEnOpt homeModule;
  inherit (lib) mkIf;
in {
  options = {
    meow.emacs.enable = mkEnOpt "Install and configure emacs.";
  };

  config = mkIf cfg.enable (homeModule ({config, ...}: {
    home.packages = with pkgs; [
      ghc
      fennel
      janet

      # org screenshot, todo make non hyprland specific and good
      grim
      slurp

      emacs-lsp-booster

      # lsp
      nixd # nix
      clang-tools # clangd + clang-format
      haskell-language-server # haskell
      fennel-ls # fennel
      nodePackages.bash-language-server # bash

      # formatters
      alejandra
    ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        config = ./config.org;
        alwaysTangle = true;
        defaultInitFile = true;

        extraEmacsPackages = epkgs:
          with epkgs; [
            use-package
            general
          ];

        package = pkgs.emacs-pgtk;
        alwaysEnsure = true;

        override = final: prev: {
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
        };
      };
    };

    home.sessionVariables = {
      EDITOR = "emacsclient -c -a ''";
    };

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
  }));
}
