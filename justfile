iso:
  nix build .#nixosConfigurations.iso.config.system.build.isoImage

prefetch:
  nix-prefetch-url --name CiscoPacketTracer_900_Ubuntu_64bit.deb https://archive.org/download/cisco-packet-tracer-900-64bit/CiscoPacketTracer_900_Ubuntu_64bit.deb
  nix-prefetch-url --name displaylink-620.zip https://www.synaptics.com/sites/default/files/exe_files/2025-09/DisplayLink%20USB%20Graphics%20Software%20for%20Ubuntu6.2-EXE.zip

_build host:
	@nix build .#nixosConfigurations.{{host}}.config.system.build.toplevel --print-out-paths --accept-flake-config --show-trace

cached_hosts := "framework server2 vps"
cache:
  #!/usr/bin/env bash
  for h in {{cached_hosts}}; do
    echo $h
    just _build $h | cachix push meowos
  done

switch host="":
  just _nh switch "{{host}}"

build host="":
  just _nh build "{{host}}"

boot host="":
  just _nh boot "{{host}}"

yeet host target="":
  #!/usr/bin/env bash
  if [[ "{{target}}" = "" ]]; then
    nh os switch -H {{host}} --target-host {{host}} . -- --accept-flake-config --show-trace
  else
    nh os switch -H {{host}} --target-host {{target}} . -- --accept-flake-config --show-trace
  fi

_nh command host:
  #!/usr/bin/env bash
  if [[ "{{host}}" = "" ]]; then
    nh os {{command}} . -- --accept-flake-config --show-trace
  else
    nh os {{command}} . -H {{host}} -- --accept-flake-config --show-trace
  fi
