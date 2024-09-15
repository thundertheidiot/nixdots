{
  lib,
  mlib,
  config,
  pkgs,
  ...
}: let
  cfg = config.meow.impermanence;
in {
  options = {
    meow.impermanence = let
      inherit (mlib) mkOpt mkEnOpt;
      inherit (lib.types) listOf str;
    in {
      enable = mkEnOpt "This impermanence module serves as a helper to using a tmpfs as your rootfs.";
      persist = mkOpt str "" {
        description = "Directory to use for persistance.";
      };

      directories = mkOpt (listOf str) [] {
        description = "Extra directories to persist.";
      };
    };
  };

  config = lib.mkIf cfg.enable (let
    mkMount = path: let
      inherit (builtins) isString isAttrs;

      mkMount' = {
        path,
        persistPath ? "${cfg.persist}/rootfs/${path}",
        permissions ? "1777",
        user ? "root",
        group ? "root",
      }: {
        inherit persistPath path permissions user group;
      };
    in
      if (isString path)
      then mkMount' {inherit path;}
      else if (isAttrs path)
      then mkMount' path
      else throw "Path provided to impermanence module is not a string or an attrset.";

    persistMounts = paths': let
      inherit (builtins) listToAttrs;

      paths = map (p: mkMount p) paths';
    in {
      systemd.services = listToAttrs (map (p:
        with p; {
          name = let
            pname =
              builtins.replaceStrings ["/"] ["_"]
              path;
          in "persist-${pname}";
          enable = true;
          value = {
            description = "Bind mount ${path}.";
            wantedBy = ["local-fs.target"];
            before = ["local-fs.target"];
            # after = ["systemd-tmpfiles-setup.service"];
            path = [pkgs.util-linux];
            unitConfig.defaultDependencies = false;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              # TODO: doesn't work after one boot??
              ExecStart = pkgs.writeShellScript "mount_${path}" ''
                mkdir --parents ${path}
                mkdir --parents ${persistPath}

                mount -o bind,X-fstrim.notrim,x-gvfs-hidden ${persistPath} ${path}
                chmod ${permissions} ${persistPath}
                chmod ${permissions} ${path}
                chown ${user}:${group} ${persistPath}
                chown ${user}:${group} ${path}
              '';
              ExecStop = "umount ${path} && rm ${path}";
            };
          };
        })
      paths);

      # systemd.tmpfiles.rules =
      #   (map (p: with p; "d ${path} ${permissions} ${user} ${group} -") paths)
      #   ++ (map (p: with p; "d ${persistPath} ${permissions} ${user} ${group} -") paths);
    };

    environmentEtcSource = loc: {
      source = "${cfg.persist}/rootfs/etc/${loc}";
    };
  in
    (persistMounts (cfg.directories
      ++ [
        {
          path = "/var/log";
          permissions = "644";
        }
        "/var/lib/bluetooth"
        # "/var/lib/nixos"
        "/root/.cache/nix"
        "/etc/NetworkManager/system-connections"
        {
          path = "/var/lib/flatpak";
          persistPath = "${cfg.persist}/flatpak";
          permissions = "755";
        }
        {
          path = "/var/lib/docker";
          persistPath = "${cfg.persist}/docker";
          permissions = "710";
        }
      ]))
    // {
      # per service config
      services.ollama.home = "/persist/ollama";

      sops.age.keyFile = "/persist/sops-key.txt";

      system.activationScripts = {
        openssh_dir.text = "mkdir --parents ${cfg.persist}/ssh";
        persist_rootfs_etc_dir.text = ''
          mkdir --parents ${cfg.persist}/rootfs/etc
        '';
      };

      services.openssh.hostKeys = [
        {
          path = "${cfg.persist}/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
        {
          path = "${cfg.persist}/ssh/ssh_host_rsa_key";
          type = "rsa";
          bits = 4096;
        }
      ];

      environment.etc = builtins.listToAttrs (builtins.map (loc: {
        name = loc;
        value = environmentEtcSource loc;
      }) ["machine-id"]);
    });
}
