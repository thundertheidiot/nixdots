{
  flake.modules.nixos.base = {
    nixpkgs.config = {
      allowUnfree = true;

      # TODO check this
      permittedInsecurePackages = [
        "libsoup-2.74.3"
      ];
    };
  };
}
