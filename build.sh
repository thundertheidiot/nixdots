#!/bin/sh

which nixos-rebuild 1> /dev/null 2> /dev/null && {
    sudo nixos-rebuild switch --flake path:#default --show-trace;
} || {
    nix run path: -- switch --flake path: --show-trace
}

