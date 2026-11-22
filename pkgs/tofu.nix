{ pkgs, ... }:
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
    exec tofu -chdir="$root/infra/tofu" "$@"
  '';
}
