{ lib, config, pkgs, ... }: {
  config = lib.mkIf (config.setup.gaming.enable) (with config; {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    environment.systemPackages = with pkgs; [
      gamescope
    ];

    programs.nix-ld.libraries = with pkgs; [
      libGL
      gperftools
    ];

    services.flatpak.enable = true;

    programs.gamemode.enable = true;

    services.joycond.enable = true;

    systemd.services."steamvr-setcap" = {
      enable = true;
      description = "Run setcap to fix steamvr.";
      unitConfig.Type = "simple";
      serviceConfig = {
        ExecStart = "${pkgs.libcap}/bin/setcap CAP_SYS_NICE+ep ${config.homeDirectory}/.local/share/Steam/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher";
      };
      wantedBy = [ "multi-user.target" ];
    };
  });
}
