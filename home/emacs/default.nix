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
          emacs-all-the-icons-fonts
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
        config = ./init.el;

        alwaysEnsure = true;
        defaultInitFile = true;

        extraEmacsPackages = epkgs:
          with epkgs; [
            treesit-grammars.with-all-grammars
          ];

        # package =
        #   if config.mHome.emacs.exwm
        #   then pkgs.emacs-gtk
        #   else pkgs.emacs-pgtk;

        package = pkgs.emacs-igc-pgtk;

        override = import ./overrides.nix {inherit pkgs;};
      };
    };
  };
}
