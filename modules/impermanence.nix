{
  lib,
  config,
  ...
}: {
  options = {
    impermanence.enable = lib.mkEnableOption "Enable the impermanence module, which turns your rootfs into a tmpfs.";

    impermanence.nixFileSystem = lib.mkOption {
      type = lib.types.attrs;
    };
  };

  config = lib.mkIf config.impermanence.enable (let
    mkPersistMount = loc: {
      device = "/nix/persist/${loc}";
      fsType = "none";
      options = ["bind"];
      depends = "/nix";
    };
    environmentEtcSource = loc: {
      ${loc}.source = "/nix/persist/etc/${loc}";
    };
  in
    with config; {
      fileSystems =
        {
          "/nix" = impermanence.nixFileSystem;
          "/" = {
            device = "none";
            fsType = "tmpfs";
            options = ["defaults" "size=64M" "mode=755"];
          };
        }
        ++ builtins.listToAttrs (builtins.map (loc: {
          name = loc;
          value = mkPersistMount loc;
        }) ["/var/log"]);

      system.activationScripts = {
        openssh_dir.text = "mkdir /nix/persist/etc/ssh";
      };

      environment.etc = builtins.listToAttrs (builtins.map (loc: {
        name = loc;
        value = environmentEtcSource loc;
      }) ["machine-id" "ssh/ssh_host_rsa_key" "ssh/ssh_host_rsa_key.pub" "ssh/ssh_host_ed25519_key" "ssh/ssh_host_ed25519_key.pub"]);
    });
}
