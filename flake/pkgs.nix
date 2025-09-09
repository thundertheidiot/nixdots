{
  inputs,
  config,
  withSystem,
  ...
}: {
  imports = [
    inputs.pkgs-by-name.flakeModule
  ];

  perSystem.pkgsDirectory = "${inputs.self.outPath}/pkgs";

  # flake = {
  #   overlays.default = final: prev:
  #     withSystem prev.stdenv.hostPlatform.system (
  #       {config, ...}: {
  #         m = config.packages;
  #       }
  #     );
  # };

  # flake.modules.nixos.mpkgs = {
  #   nixpkgs.overlays = [
  #     inputs.self.overlays.default
  #   ];
  # };
}
