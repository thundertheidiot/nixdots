{
  inputs,
  config,
  ...
}: {
  imports = [
    inputs.pkgs-by-name.flakeModule
  ];

  perSystem.pkgsDirectory = "${config.flake.root}/pkgs";
}
