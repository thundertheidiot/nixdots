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
    meow.emacs.exwm = mkEnOpt "Install and configure EXWM.";
  };

  config = mkIf cfg.enable ({
      environment.variables = lib.mkIf cfg.exwm {
        EMACS_ENABLE_EXWM = "1"; # used inside emacs
      };

      services.xserver.displayManager.session = lib.mkIf cfg.exwm [
        {
          manage = "desktop";
          name = "EXWM";
          start = ''
            exec ${pkgs.dbus}/bin/dbus-launch --exit-with-session emacs -mm &
            waitPID=$!
          '';
        }
      ];

      services.xserver.enable = lib.mkDefault cfg.exwm;
      services.xserver.displayManager.startx.enable = cfg.exwm;

      environment.systemPackages = lib.mkIf cfg.exwm (with pkgs; [
        wmctrl
      ]);

      services.ollama.enable = true;
    }
    // homeModule ({config, ...}: {
      # systemd.user.targets.exwm-session = lib.mkIf cfg.exwm {
      #   Unit = {
      #     Description = "EXWM session";
      #     BindsTo = ["graphical-session.target"];
      #     Wants = ["graphical-session-pre.target"];
      #     After = ["graphical-session-pre.target"];
      #   };
      # };

      home.packages = with pkgs; [
        ghc
        fennel
        janet
        sbcl

        # org screenshot, todo make non hyprland specific and good
        grim
        slurp

        # latex
        texlive.combined.scheme-full

        emacs-lsp-booster

        # lsp
        nixd # nix
        clang-tools # clangd + clang-format
        haskell-language-server # haskell
        fennel-ls # fennel
        nodePackages.bash-language-server # bash
        pyright # python

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

          package = pkgs.emacs-gtk;
          alwaysEnsure = true;

          override = final: prev: {
            dwm-workspaces = final.trivialBuild {
              pname = "dwm-workspaces";
              version = "1.0";

              src = ./dwm-workspaces.el;

              recipe = pkgs.writeText "recipe" ''
                (dwm-workspaces :fetcher github :repo "doesnt/exist" :files (:defaults "dwm-workspaces.el"))
              '';
            };
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
