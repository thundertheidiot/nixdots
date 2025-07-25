{
  lib,
  config,
  mlib,
  pkgs,
  ...
}: let
  cfg = config.meow.virtualization;

  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;
in {
  options = {
    meow.virtualization = {
      enable = mkEnOpt "Install and configure virt-manager.";
    };
  };

  config = mkIf cfg.enable {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;

    users.users."${config.meow.user}".extraGroups = ["libvirtd"];

    meow.impermanence.directories = [
      {
        path = "/var/lib/libvirt";
        persistPath = "${config.meow.impermanence.persist}/libvirt";
        permissions = "750";
      }
    ];

    systemd.services."virsh-net" = {
      enable = true;
      description = "Start virbr0 network bridge on boot.";
      unitConfig = {
        Type = "simple";
      };
      serviceConfig = {
        ExecStart = "${pkgs.libvirt}/bin/virsh net-start default";
        RemainAfterExit = true;
      };
      wantedBy = ["multi-user.target"];
    };
  };
}
