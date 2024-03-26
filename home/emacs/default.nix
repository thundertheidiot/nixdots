{
  config,
  pkgs,
  inputs,
  ...
}:
with config; {
  home.packages = with pkgs; [
    nil # nix language server
  ];

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
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs: [
      pkgs.emacs-lsp-booster
      epkgs.eglot-booster
      epkgs.indent-bars

      epkgs.diminish
      epkgs.undo-tree
      epkgs.evil
      epkgs.evil-collection
      epkgs.evil-better-visual-line
      epkgs.smartparens
      epkgs.evil-smartparens
      epkgs.which-key
      epkgs.general

      epkgs.org-bullets
      epkgs.solaire-mode
      epkgs.catppuccin-theme
      epkgs.hl-todo
      epkgs.flymake
      epkgs.eglot
      epkgs.company
      epkgs.company-box
      epkgs.treesit-auto

      epkgs.rustic
      epkgs.lua-mode
      epkgs.gdscript-mode
      epkgs.nix-mode
      epkgs.rainbow-delimiters

      epkgs.projectile
      epkgs.ibuffer-projectile
      epkgs.magit
      epkgs.vterm
      epkgs.eshell-vterm
      epkgs.fish-completion
      epkgs.vertico
      epkgs.orderless
      epkgs.consult
      epkgs.marginalia
      epkgs.popper
      epkgs.simple-mpc
      epkgs.empv
    ];
  };

  services.emacs = {
    enable = true;
    client.enable = true;
    defaultEditor = true;
    startWithUserSession = true;
  };
}
