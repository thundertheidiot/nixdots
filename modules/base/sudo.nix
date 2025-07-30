{
  flake.modules.nixos.base = {lib, ...}: {
    security.sudo.enable = lib.mkForce false;
    security.sudo-rs = {
      enable = true;
      execWheelOnly = true;
    };
  };
}
