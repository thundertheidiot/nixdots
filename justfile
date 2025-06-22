iso:
  nix build .#nixosConfigurations.iso.config.system.build.isoImage

build-flakeless HOST:
  nix-build --argstr host {{HOST}} --attr config.system.build.toplevel

switch host="":
  just _nh switch "{{host}}"

yeet host target="":
  #!/usr/bin/env bash
  if [[ "{{target}}" = "" ]]; then
    nh os switch -H {{host}} --target-host {{host}} . -- --accept-flake-config --show-trace
  else
    nh os switch -H {{host}} --target-host {{target}} . -- --accept-flake-config --show-trace
  fi

build host="":
  just _nh build "{{host}}"

boot host="":
  just _nh boot "{{host}}"

_nh command host:
  #!/usr/bin/env bash
  if [[ "{{host}}" = "" ]]; then
    nh os {{command}} . -- --accept-flake-config --show-trace
  else
    nh os {{command}} . -H {{host}} -- --accept-flake-config --show-trace
  fi