#!/bin/sh

nixos() {
    sudo nixos-rebuild switch --flake path:.#local --show-trace
    exit 0 # Otherwise home will run on failure
}

home() {
    nix run path: -- switch --flake path:. --show-trace
}

which nixos-rebuild 1> /dev/null 2> /dev/null && nixos || home
