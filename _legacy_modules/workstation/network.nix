{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf;
  cfg = config.meow.workstation.network.enable;
in {
  options = {
    meow.workstation.network.enable = mkEnOptTrue "Enable workstation specific network configuration.";
  };

  config = mkIf cfg {
    environment.systemPackages = with pkgs; [
      wireguard-tools
    ];

    # needed for vpns
    networking.firewall.checkReversePath = false;

    systemd.services."NetworkManager-wait-online".enable = false;
  };
}
