{ pkgs, customFiles, ... }: {
  programs.emacs = {
    enable = true;
    # package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.diminish
      epkgs.undo-fu
      epkgs.evil
      epkgs.evil-collection
      epkgs.evil-better-visual-line
      epkgs.smartparens
      epkgs.evil-smartparens
      epkgs.which-key
      epkgs.general

      epkgs.org-bullets
      epkgs.solaire-mode
      epkgs.doom-themes
      epkgs.hl-todo
      epkgs.flymake
      epkgs.eglot
      epkgs.company
      epkgs.company-box
      epkgs.treesit-auto

      epkgs.rustic
      epkgs.lua-mode
      epkgs.csharp-mode
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
      # epkgs.savehist
      epkgs.orderless
      epkgs.consult
      epkgs.marginalia
      epkgs.popper
      epkgs.simple-mpc
      epkgs.empv
    ];
  };

  home.file.".emacs.d" = {
    source = ./config;
    recursive = true;
  };

  # home.file."test.el" = {
  #   source = "${customFiles.emacs-eglot-booster}/eglot-booster.el";
  # };
}
