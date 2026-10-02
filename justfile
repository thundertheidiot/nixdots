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
  just _rebuild switch "{{host}}"

build host="":
  just _rebuild build "{{host}}"

boot host="":
  just _rebuild boot "{{host}}"

yeet host target="":
  #!/usr/bin/env bash
  target="{{target}}"
  if [[ "$target" = "" ]]; then
    target="{{host}}"
  fi
  if [[ -n "${INSIDE_EMACS:-}" && "$INSIDE_EMACS" != vterm ]]; then
    nixos-rebuild switch --flake ".#{{host}}" --target-host "$target" --sudo --accept-flake-config --show-trace --no-reexec
  else
    nh os switch -H "{{host}}" --target-host "$target" . -- --accept-flake-config --show-trace
  fi

_rebuild command host:
  #!/usr/bin/env bash
  host="{{host}}"
  if [[ -n "${INSIDE_EMACS:-}" && "$INSIDE_EMACS" != vterm ]]; then
    flake="."
    if [[ -n "$host" ]]; then
      flake=".#$host"
    fi
    nixos-rebuild "{{command}}" --flake "$flake" --accept-flake-config --show-trace --sudo --no-reexec
  elif [[ -z "$host" ]]; then
    nh os "{{command}}" . -- --accept-flake-config --show-trace
  else
    nh os "{{command}}" . -H "$host" -- --accept-flake-config --show-trace
  fi

[positional-arguments]
wg *args:
  nix run .#wireguard-keys -- "$@"
