{ lib, pkgs, ... }:
let
  inherit (builtins) toJSON;
  inherit (lib) filterAttrs mapAttrs;

  infra = import ../infra;
  wg = infra.networks.wg;
  inventory = pkgs.writeText "wireguard-inventory.json" (toJSON {
    inherit (wg) hub port mtu;
    endpoint = "${infra.hosts.${wg.hub}.public.v4}:${toString wg.port}";
    dns = infra.dns.resolvers.wg;
    peers = mapAttrs (_: host: host.wg) (filterAttrs (_: host: host ? wg) infra.hosts);
  });
in
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
    exec python3 ${../infra/wireguard/keys.py} --inventory ${inventory} "$@"
  '';
}
