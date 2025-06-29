{
  flake.modules.nixos.base = {
    boot = {
      tmp = {
        cleanOnBoot = true;
      };

      loader.grub.enable = true;
    };
  };
}
