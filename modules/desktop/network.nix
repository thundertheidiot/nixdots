{
  flake.modules.nixos.desktop = {pkgs, ...}: {
    environment.systemPackages = with pkgs; [
      wireguard-tools
    ];

    networking.networkmanager.enable = true;

    # needed for vpns
    networking.firewall.checkReversePath = false;

    systemd.services."NetworkManager-wait-online".enable = false;
  };
}
