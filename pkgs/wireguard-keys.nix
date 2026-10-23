{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "wireguard-keys";
  runtimeInputs = with pkgs; [
    python3
    wireguard-tools
    sops
    qrencode
  ];
  text = ''
    exec python3 ${../sops/wireguard/keys.py} "$@"
  '';
}
