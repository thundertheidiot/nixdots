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

    # programs.emacs.overrides = self: super: {
    #   indent-bars = self.trivialBuild {
    #     pname = "indent-bars";
    #     version = "1.0.0";

    #     packageRequires = with pkgs.emacsPackages; [
    #       compat
    #     ];

    #     src = pkgs.fetchgit {
    #       url = "https://github.com/jdtsmith/indent-bars";
    #       rev = "4583e3e9f507143cd4241131b77fc5e8b1722bbf";
    #       sha256 = "sha256:12c37pfmf3x1r9z8fv19xgf4nsir7a65l52r46a6gk2vd1dwz7fj";
    #     };
    #     recipe = pkgs.writeText "recipe" ''
    #       (indent-bars :fetcher github :repo "jdtsmith/indent-bars" :files (:defaults "indent-bars.el"))
    #     '';
    #   };

    #   smartparens = self.trivialBuild {
    #     pname = "smartparens";
    #     version = "1.0.0";

    #     packageRequires = with pkgs.emacsPackages; [
    #       dash
    #     ];

    #     src = pkgs.fetchgit {
    #       url = "https://github.com/Fuco1/smartparens";
    #       rev = "d3b616843167f04b8a9f53dd25e84818c9f6fbce";
    #       sha256 = "sha256-ldt0O9nQP3RSsEvF5+irx6SRt2GVWbIao4IOO7lOexM=";
    #     };
    #     recipe = pkgs.writeText "recipe" ''
    #       (smartparens :fetcher github :repo "Fuco1/smartparens")
    #     '';
    #   };
    # };

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

        package = pkgs.emacs29-pgtk;
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
