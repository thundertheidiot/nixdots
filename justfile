iso:
  nix build .#nixosConfigurations.iso.config.system.build.isoImage

prefetch:
  nix-prefetch-url --name CiscoPacketTracer822_amd64_signed.deb https://www.netacad.com/authoring-resources/courses/ff9e491c-49be-4734-803e-a79e6e83dab1/c3636211-1ce6-4f92-8a22-ccddf902dd72/en-US/assets/PacketTracer822_amd64_signed_en-US_35234a27-3127-49bc-91ce-2926af76f07a.deb
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
