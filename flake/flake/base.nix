{inputs, ...}: {
  flake.root = inputs.self.outPath;

  imports = [
    inputs.flake-parts.flakeModules.modules
  ];
}
