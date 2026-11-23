{ lib, pkgs, ... }:
let
  infra = import ../infra { inherit lib; };
  inherit (infra.hosts.vps) public;
in
pkgs.writeShellApplication {
  name = "tofu";
  passthru.autoUpdate.skip = "Local script; dependencies follow flake.lock.";
  runtimeInputs = with pkgs; [
    git
    opentofu
    sops
  ];
  text = ''
    root=$(git rev-parse --show-toplevel)
    set -a
    # shellcheck disable=SC1090
    source <(sops decrypt "$root/infra/porkbun.env")
    set +a
    export TF_VAR_vps_v4=${public.v4} TF_VAR_vps_v6=${public.v6}
    exec tofu -chdir="$root/infra/tofu" "$@"
  '';
}
