{
  flake.modules.nixos.base = {
    boot.initrd.systemd.enable = true;
    boot.initrd.systemd.settings.Manager = {
      DefaultTimeoutStopSec = "3s";
    };
  };
}
