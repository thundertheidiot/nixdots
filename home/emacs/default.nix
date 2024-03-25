{
  config,
  pkgs,
  inputs,
  ...
}: let
  emacsPath = ".local/share/emacs/extrapkgs";
in with config; {
  home.file."${emacsPath}/eglot-booster.el" = {
    source = "${inputs.emacs-eglot-booster}/eglot-booster.el";
  };
  home.file."${emacsPath}/indent-bars.el" = {
    source = "${inputs.emacs-indent-bars}/indent-bars.el";
  };

  home.packages = with pkgs; [
    nil
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs: [
      pkgs.emacs-lsp-booster
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
