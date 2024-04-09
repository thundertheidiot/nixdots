{ lib, config, pkgs, ... }: {
  config = lib.mkIf (config.setup.desktop.enable) {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;

    programs.dconf = {
      enable = true;
      # settings = {
      #   "org/virt-manager/virt-manager/connections" = {
      #     autoconnect = ["qemu:///system"];
      #     uris = ["qemu:///system"];
      #   };
      # };
    };

    environment.systemPackages = with pkgs; [
      android-tools
    ];

    programs.adb.enable = true;
    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

    users.users.${config.username}.extraGroups = [ "libvirtd" "adbusers" ];

    systemd.services."virsh-net" = {
      enable = true;
      description = "Start virbr0 network bridge on boot.";
      unitConfig = {
        Type = "simple";
      };
      serviceConfig = {
        ExecStart = "${pkgs.libvirt}/bin/virsh net-start default";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
