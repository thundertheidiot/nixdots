{ lib, pkgs, ... }:
let
  inherit (builtins) toJSON;
  inherit (lib) filterAttrs mapAttrs;

  infra = import ../infra { inherit lib; };
  inherit (infra.networks) wg;
  inventory = pkgs.writeText "wireguard-inventory.json" (toJSON {
    inherit (wg) hub mtu endpoint;
    dns = infra.dns.resolvers.wg;
    peers = mapAttrs (_: host: host.wg) (filterAttrs (_: host: host ? wg) infra.hosts);
  });
in
pkgs.writeShellApplication {
  name = "wireguard-keys";
  passthru.autoUpdate.skip = "Local script; dependencies follow flake.lock.";
  runtimeInputs = with pkgs; [
    git
    python3
    wireguard-tools
    sops
    qrencode
  ];
  text = ''
    root=$(git rev-parse --show-toplevel)
    exec python3 ${../infra/wireguard/keys.py} --directory "$root/infra/wireguard" --inventory ${inventory} "$@"
  '';
}
