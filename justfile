iso:
  nix build .#nixosConfigurations.iso.config.system.build.isoImage

build-flakeless HOST:
  nix-build --argstr host {{HOST}} --attr config.system.build.toplevel

switch:
  nh os switch . -- --accept-flake-config