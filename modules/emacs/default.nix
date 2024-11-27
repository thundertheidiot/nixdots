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

      # TODO: move all this shit to modules/langs or something
      lang = {
        latex = mkEnOpt "Latex support";
        haskell = mkEnOpt "Haskell";
        rust = mkEnOpt "Rust";
        ocaml = mkEnOpt "Ocaml";
        fennel = mkEnOpt "Fennel";
        janet = mkEnOpt "Janet";
        c_cxx = mkEnOpt "C/C++";
        bash = mkEnOpt "Bash";
        python = mkEnOpt "Python";
      };
    };
  };

  config = mkIf cfg.enable ({
      environment.variables = lib.mkIf cfg.exwm {
        EMACS_ENABLE_EXWM = "1"; # used inside emacs
        EDITOR = "emacsclient -c -a ''";
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
      home.packages = with pkgs;
        lib.mkMerge [
          [
            # org screenshot, todo make non hyprland specific and good
            grim
            slurp

            emacs-lsp-booster

            # lsp
            nixd # nix

            # formatters
            alejandra
          ]
          (mkIf cfg.lang.rust [
            # pkgs.rust-bin.stable.default
            # (pkgs.rust-bin.selectLatestNightlyWith (toolchain:
            #   toolchain.default.override {
            #     extensions = ["rust-analyzer"];
            #     targets = ["x86_64-unknown-linux-gnu"];
            #   }))
          ])
          (mkIf cfg.lang.haskell [
            (haskellPackages.ghcWithPackages (pkgs: with pkgs; [stack]))
            haskell-language-server
          ])
          (mkIf cfg.lang.ocaml [
            ocaml
            opam
            dune_3
            ocamlPackages.utop
            ocamlPackages.merlin
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
          ])
          (mkIf cfg.lang.janet [
            janet
            (jpm.overrideAttrs (prev: {
              buildInputs = prev.buildInputs ++ [makeWrapper];

              installPhase =
                prev.installPhase
                + ''
                  wrapProgram $out/bin/jpm --add-flags '--tree="$JANET_TREE" --binpath="$XDG_DATA_HOME/janet/bin" --headerpath=${janet}/include --libpath=${janet}/lib --ldflags=-L${pkgs.glibc}/lib'
                '';
            }))
          ])
          (mkIf cfg.lang.latex [
            texlive.combined.scheme-full
          ])
          (mkIf cfg.lang.c_cxx [
            clang-tools
          ])
          (mkIf cfg.lang.bash [
            nodePackages.bash-language-server
          ])
          (mkIf cfg.lang.fennel [
            fennel
            fennel-ls
          ])
          (mkIf cfg.lang.python [
            pyright
          ])
        ];

      services.emacs = {
        enable = true;
        defaultEditor = true;
        # client.enable = true;
        startWithUserSession = "graphical";
      };

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
            sqlite_vss = "${pkgs."2405".sqlite-vss}/lib/vss0.so";
            lang_latex = tangle cfg.lang.latex;
            lang_haskell = tangle cfg.lang.haskell;
            lang_ocaml = tangle cfg.lang.ocaml;
            lang_fennel = tangle cfg.lang.fennel;
            lang_janet = tangle cfg.lang.janet;
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

              treesit-grammars.with-all-grammars # TODO: split up
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
        # EDITOR = lib.mkForce "emacsclient -c -a ''";
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
