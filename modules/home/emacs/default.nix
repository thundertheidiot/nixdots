{
  config,
  pkgs,
  lib,
  ...
}: {
  config = lib.mkIf (config.workstation.enable) (with config; {
    home.packages = with pkgs; [
      nil # nix language server
      clang-tools # clangd + clang-format
      haskell-language-server ghc
    ];

    services.pantalaimon = {
      enable = false;
      settings = {
        Default = {
          LogLevel = "Debug";
          SSL = true;
        };
        Kotiboksi = {
          Homeserver = "https://matrix.kotiboksi.xyz";
          ListenAddress = "127.0.0.1";
          ListenPort = "8008";
        };
      };
    };

    programs.emacs.overrides = self: super: {
      eglot-booster = self.trivialBuild {
        pname = "eglot-booster";
        version = "1.0.0";

        src = pkgs.fetchgit {
          url = "https://github.com/jdtsmith/eglot-booster";
          rev = "caee55ee5285659964d0b9fe4101e28de09701ca";
          sha256 = sha256:00c96vhmmxx9dspkqk6jir0y9nwb32zbf5ixqjjdzrjy0kh65ii8;
        };
        recipe = pkgs.writeText "recipe" ''
          (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster" :files (:defaults "eglot-booster.el"))
        '';
      };

      indent-bars = self.trivialBuild {
        pname = "indent-bars";
        version = "1.0.0";

        packageRequires = with pkgs.emacsPackages; [
          compat
        ];

        src = pkgs.fetchgit {
          url = "https://github.com/jdtsmith/indent-bars";
          rev = "4583e3e9f507143cd4241131b77fc5e8b1722bbf";
          sha256 = sha256:12c37pfmf3x1r9z8fv19xgf4nsir7a65l52r46a6gk2vd1dwz7fj;
        };
        recipe = pkgs.writeText "recipe" ''
          (indent-bars :fetcher github :repo "jdtsmith/indent-bars" :files (:defaults "indent-bars.el"))
        '';
      };

      catppuccin-theme = self.trivialBuild {
        pname = "catppuccin-theme";
        version = "1.0.0";

        src = pkgs.fetchgit {
          url = "https://github.com/catppuccin/emacs";
          rev = "3d93abaa33e95f19b4a8b0e1e9bef1e3e68dd994";
          sha256 = sha256:1j6nsy9is067288x2riabb7kc3grghb2g7bkvwndn2jyglbbxgi0;
        };
        recipe = pkgs.writeText "recipe" ''
          (catppuccin-theme :fetcher github :repo "catppuccin/emacs" :files ("catppuccin-theme.el"))
        '';
      };

      smartparens = self.trivialBuild {
        pname = "smartparens";
        version = "1.0.0";

        packageRequires = with pkgs.emacsPackages; [
          dash
        ];

        src = pkgs.fetchgit {
          url = "https://github.com/Fuco1/smartparens";
          rev = "d3b616843167f04b8a9f53dd25e84818c9f6fbce";
          sha256 = "sha256-ldt0O9nQP3RSsEvF5+irx6SRt2GVWbIao4IOO7lOexM=";
        };
        recipe = pkgs.writeText "recipe" ''
          (smartparens :fetcher github :repo "Fuco1/smartparens")
        '';
      };
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
      extraConfig = builtins.readFile ./init.el;
      extraPackages = epkgs:
        with epkgs; [
          pkgs.emacs-lsp-booster
          eglot-booster
          indent-bars

          diminish
          undo-tree
          evil
          evil-collection
          evil-better-visual-line
          smartparens
          evil-smartparens
          which-key
          general

          org-bullets
          solaire-mode
          catppuccin-theme
          hl-todo
          flymake
          eglot
          company
          company-box
          treesit-auto

          rustic
          lua-mode
          gdscript-mode
          nix-mode
          haskell-mode
          rainbow-delimiters

          projectile
          ibuffer-projectile
          magit
          vterm
          eshell-vterm
          fish-completion
          vertico
          orderless
          consult
          marginalia
          popper
          simple-mpc
          empv

          ement
        ];
    };

    services.emacs = {
      enable = true;
      client.enable = true;
      defaultEditor = true;
      startWithUserSession = true;
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
      text = "This file is here to make nix create \"${xdg.configHome}/emacs/\", so emacs uses it instead of \"${home.homeDirectory}/.emacs.d/\".";
    };
  });
}
