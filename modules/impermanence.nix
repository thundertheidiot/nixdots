{
  lib,
  mlib,
  config,
  ...
}: let
  cfg = config.meow.impermanence;
in {
  options = {
    meow.impermanence = let
      inherit (mlib) mkOpt mkEnOpt;
      inherit (lib.types) str;
    in {
      enable = mkEnOpt "This impermanence module serves as a helper to using a tmpfs as your rootfs.";
      persist = mkOpt str "" {
        description = "Directory to use for persistance.";
      };
    };
  };

  config = lib.mkIf cfg.enable (let
    mkPersistMount = loc: {
      device = "${cfg.persist}/${loc}";
      fsType = "none";
      options = ["bind"];
      depends = "/${cfg.persist}";
    };

    persistMounts = paths: let
      inherit (builtins) listToAttrs;
      inherit (lib.strings) concatStringsSep;
    in {
      fileSystems = listToAttrs (map (p: {
          name = p;
          value = {
            device = let
              inherit (builtins) replaceStrings;
              path = replaceStrings ["/"] ["_"] p;
            in "${cfg.persist}/${path}";
            fsType = "none";
            options = ["bind"];
            depends = "/${cfg.persist}";
          };
        })
        paths);

      # boot.initrd.postMountCommands = concatStringsSep "\n" (map (p: "mkdir --parents ${cfg.persist}/${p}") paths);

      # system.activationScripts = listToAttrs (map (p: {
      #     name = "${p}_create";
      #     value = {text = "mkdir --parents ${cfg.persist}/${p}";};
      #   })
      #   paths);
    };

    environmentEtcSource = loc: {
      source = "${cfg.persist}/etc/${loc}";
    };
  in
    (persistMounts ["/var/log"])
    // {
      # fileSystems =
      #   # {
      #   #   "/nix" = impermanence.nixFileSystem;
      #   #   "/" = {
      #   #     device = "none";
      #   #     fsType = "tmpfs";
      #   #     options = ["defaults" "size=64M" "mode=755"];
      #   #   };
      #   # }
      #   # ++
      #   builtins.listToAttrs (builtins.map (loc: {
      #     name = loc;
      #     value = mkPersistMount loc;
      #   }) ["/var/log"]);

      system.activationScripts = {
        openssh_dir.text = "mkdir ${cfg.persist}/etc/ssh";
        # var_log_dir.text = "mkdir ${cfg.persist}/var/log";
      };

      environment.etc = builtins.listToAttrs (builtins.map (loc: {
        name = loc;
        value = environmentEtcSource loc;
      }) ["machine-id" "ssh/ssh_host_rsa_key" "ssh/ssh_host_rsa_key.pub" "ssh/ssh_host_ed25519_key" "ssh/ssh_host_ed25519_key.pub"]);
    });
}
