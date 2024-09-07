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
    meow.emacs = {
      enable = mkEnOpt "Install and configure emacs.";
      exwm = mkEnOpt "Install and configure EXWM.";
      llm = mkEnOpt "Install llm interaction tools for emacs.";

      lang = {
        latex = mkEnOpt "Latex support";
        haskell = mkEnOpt "Haskell";
        fennel = mkEnOpt "Fennel";
        c_cxx = mkEnOpt "C/C++";
        bash = mkEnOpt "Bash";
        python = mkEnOpt "Python";
      };
    };
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

      services.ollama.enable = lib.mkDefault cfg.llm;
    }
    // homeModule ({config, ...}: {
      home.packages = with pkgs; [
        (lib.mkIf cfg.lang.haskell ghc)
        (lib.mkIf cfg.lang.fennel fennel)

        # org screenshot, todo make non hyprland specific and good
        grim
        slurp

        # latex
        (lib.mkIf cfg.lang.latex texlive.combined.scheme-full)

        emacs-lsp-booster

        # lsp
        nixd # nix
        (lib.mkIf cfg.lang.c_cxx clang-tools)
        (lib.mkIf cfg.lang.haskell haskell-language-server)
        (lib.mkIf cfg.lang.fennel fennel-ls)
        (lib.mkIf cfg.lang.bash nodePackages.bash-language-server)
        (lib.mkIf cfg.lang.python pyright)

        # formatters
        alejandra
      ];

      programs.emacs = {
        enable = true;
        package = pkgs.emacsWithPackagesFromUsePackage {
          config = pkgs.substituteAll (let
            tangle = cfg:
              if cfg
              then "yes"
              else "no";
          in {
            src = ./config.org;

            exwm_enable = tangle cfg.exwm;
            llm_enable = tangle cfg.llm;
            lang_latex = tangle cfg.lang.latex;
            lang_haskell = tangle cfg.lang.haskell;
            lang_fennel = tangle cfg.lang.fennel;
            lang_c_cxx = tangle cfg.lang.c_cxx;
            lang_bash = tangle cfg.lang.bash;
            lang_python = tangle cfg.lang.python;
          });
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
        # Somehow, somewhere, this is set to nano. Where? I have no clue.
        EDITOR = lib.mkForce "emacsclient -c -a ''";
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
