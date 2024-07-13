let
  modules = [
  ];
  kodiDir = config: "${config.homeDirectory}/.local/share/kodi/";
in {
  system = {
    config,
    pkgs,
    lib,
    mlib,
    inputs,
    ...
  }: {
    imports = mlib.getSystems modules;

    config = lib.mkIf (config.tv.enable) {
      # services.displayManager.sddm = {
      #   settings = {
      #     Autologin = {
      #       Session = "hyprland.desktop";
      #       User = "${config.username}";
      #     };
      #   };
      # };

      # systemd.services."ir-client" = let
      #   naersk = pkgs.callPackage inputs.naersk {};
      #   ir-client = naersk.buildPackage {
      #     src = ./ir-client;
      #   };
      # in {
      #   enable = true;
      #   description = "Use tv remote as an input.";
      #   unitConfig = {
      #     Type = "simple";
      #   };
      #   serviceConfig = {
      #     ExecStart = "${ir-client}/bin/ir-client";
      #   };
      #   wantedBy = ["multi-user.target"];
      # };
    };
  };

  home = {
    config,
    pkgs,
    lib,
    mlib,
    ...
  }: {
    imports = mlib.getHomes modules;

    config = lib.mkIf (config.tv.enable) {
      home.packages = [
        (import ./kodi/settings.nix {inherit config lib pkgs;})
      ];

      programs.kodi = {
        enable = true;
        datadir = kodiDir config;
      };
    };
  };
}
