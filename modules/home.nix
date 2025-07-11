{
  config,
  inputs,
  ...
}: {
  flake.modules.nixos.home = {...}: {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      backupFileExtension = "hm_backup";
    };
  };
}
