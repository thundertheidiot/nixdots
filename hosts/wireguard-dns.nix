{ lib, pkgs, ... }:
let
  inherit (lib) getExe;

  updateDns' = pkgs.writeShellApplication {
    name = "wireguard-dns";
    runtimeInputs = with pkgs; [
      iproute2
      networkmanager
      gnugrep
      systemd
    ];
    text = ''
      ip link show wg0 > /dev/null 2>&1 || exit 0

      for device in $(nmcli -t -g DEVICE device status); do
        if [ "$device" != wg0 ] &&
          nmcli -g IP4.DNS device show "$device" | grep -Fq 192.168.101.111; then
          resolvectl revert wg0
          exit 0
        fi
      done

      resolvectl dns wg0 10.100.0.1
      resolvectl domain wg0 '~home' '~server'
      resolvectl default-route wg0 false
    '';
  };

  updateDns = getExe updateDns';
in
{
  services.resolved.enable = true;
  networking.networkmanager.dns = "systemd-resolved";
  networking.networkmanager.dispatcherScripts = [
    {
      source = updateDns;
      type = "basic";
    }
  ];

  networking.wg-quick.interfaces.wg0 = {
    postUp = "${updateDns}";
    preDown = ''
      ${pkgs.systemd}/bin/resolvectl revert wg0 || true
    '';
  };
}
