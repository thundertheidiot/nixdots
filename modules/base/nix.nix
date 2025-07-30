{config, ...}: let
  inherit (config.flake) root;
in {
  flake.modules.nixos.base = {pkgs, ...}: {
    # doesn't work on lix, doesn't work with flakes anyway?
    system.tools.nixos-option.enable = false;

    nix.package = pkgs.lixPackageSets.latest.lix;

    nix.settings =
      {
        experimental-features = ["nix-command" "flakes"];
        use-xdg-base-directories = true;
        allow-import-from-derivation = true;
      }
      # Is this stupid? Yes, unfortunately flakes are stupid too, and the attributes cannot be computed, but i also want a single source of truth for these
      # https://github.com/NixOS/nix/issues/4945
      # TODO the real question here is if this is ever needed in practice
      // (import "${root}/flake.nix").nixConfig;
  };
}
