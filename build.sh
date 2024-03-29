#!/bin/sh

nixos() {
    sudo nixos-rebuild switch --flake path:#default --show-trace
    exit 0 # Otherwise home will run on failure
}

home() {
    nix run path: -- switch --flake path: --show-trace
}

nix flake lock --update-input custom-kodi

which nixos-rebuild 1> /dev/null 2> /dev/null && nixos || home
