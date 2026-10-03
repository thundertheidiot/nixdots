{
  config,
  pkgs,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf;

  work = config.meow.workstation.enable;
  cfg = config.meow.workstation.network.enable;
in
{
  options = {
    meow.workstation.network.enable = mkEnOptTrue "Enable workstation specific network configuration.";
  };

  config = mkIf (work && cfg) {
    networking.networkmanager.enable = true;

    environment.systemPackages = with pkgs; [
      wireguard-tools
    ];

    # needed for vpns
    networking.firewall.checkReversePath = false;

    systemd.services."NetworkManager-wait-online".enable = false;
  };
}
