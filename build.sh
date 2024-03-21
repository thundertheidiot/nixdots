#!/bin/sh

which nixos-rebuild 1> /dev/null 2> /dev/null && {
    sudo nixos-rebuild switch --flake path:#default;
} || {
    nix run path: -- switch --flake path:
}

# TODO: make good
#sudo nixos-rebuild switch --flake path:#default --impure
