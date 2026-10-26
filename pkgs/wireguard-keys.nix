{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "wireguard-keys";
  passthru.autoUpdate.skip = "Local script; dependencies follow flake.lock.";
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
