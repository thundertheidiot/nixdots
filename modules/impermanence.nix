{
  lib,
  mlib,
  config,
  pkgs,
  ...
}: let
  cfg = config.meow.impermanence;

  inherit (lib) mkIf mkMerge;
in {
  options = {
    meow.impermanence = let
      inherit (mlib) mkOpt mkEnOpt;
      inherit (lib.types) listOf str attrs;
    in {
      enable = mkEnOpt "This impermanence module serves as a helper to using a tmpfs as your rootfs.";
      persist = mkOpt str "" {
        description = "Directory to use for persistance.";
      };

      directories = mkOpt (listOf attrs) [] {
        description = "Extra directories to persist.";
      };

      ensureDirectories = mkOpt (listOf str) [] {
        description = "Directories to ensure creation of.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # Ensure directory creation
    {
      system.activationScripts = let
        inherit (builtins) concatStringsSep;
      in {
        ensure_directories = concatStringsSep "\n" (
          map
          (dir: "mkdir --parents \"${dir}\"")
          cfg.ensureDirectories
        );
      };
    }
    # Directories
    (let
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

        paths = map (p: mkMount p) (builtins.filter (i: i != null) paths');
      in
        listToAttrs (map (p:
          with p; {
            name = let
              pname =
                builtins.replaceStrings ["/"] ["_"]
                path;
            in "persist-${pname}";
            enable = true;
            value = {
              description = "Bind mount ${path}.";
              wantedBy =
                if config.meow.workstation.enable
                then ["graphical.target"]
                else ["multi-user.target"];
              before =
                if config.meow.workstation.enable
                then ["graphical.target"]
                else ["multi-user.target"];
              path = [pkgs.util-linux];
              unitConfig.DefaultDependencies = false;
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
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

      environmentEtcSource = loc: {
        source = "${cfg.persist}/rootfs/etc/${loc}";
      };
    in {
      systemd.services = persistMounts (cfg.directories
        ++ [
          {
            path = "/var/log";
            permissions = "711";
          }
          "/var/lib/bluetooth"
          # "/var/lib/nixos"
          "/root/.cache/nix"
          "/etc/NetworkManager/system-connections"
          "/var/lib/systemd"
          # {
          #   path = "/var/lib/flatpak";
          #   persistPath = "${cfg.persist}/flatpak";
          #   permissions = "755";
          # }
          {
            path = "/var/lib/docker";
            persistPath = "${cfg.persist}/docker";
            permissions = "710";
          }
          {
            path = "/var/lib/containers";
            persistPath = "${cfg.persist}/containers";
            permissions = "710";
          }
          "/var/lib/fwupd"
          "/var/cache/fwupd"
          (
            if config.services.fprintd.enable
            then {
              path = "/var/lib/fprint";
            }
            else null
          )
          # (
          #   if config.services.displayManager.sddm.enable
          #   then {
          #     path = "/var/lib/sddm";
          #     permissions = "750";
          #     user = "sddm";
          #     group = "sddm";
          #   }
          #   else null
          # )
        ]);

      # logrotate permission fix for updates
      systemd.tmpfiles.rules = [
        "d /var/log 0711 root root - -"
      ];

      environment.etc = builtins.listToAttrs (builtins.map (loc: {
        name = loc;
        value = environmentEtcSource loc;
      }) ["machine-id"]);
    })
    # /etc/shadow
    (let
      pShadow = "${cfg.persist}/rootfs/etc/shadow";
    in {
      system.activationScripts = {
        # The first copy accounts for reactivation after startup, this example scenario should explain that
        # 1. User starts up their computer
        # 2. ${pShadow} is copied over /etc/shadow
        # 3. User changes their password
        # 4. User updates their system, reactivating the configuration
        # 5. The old unchanged ${pShadow} is copied over /etc/shadow
        # 6. User is very confused, as their password has changed back
        etc_shadow = ''
          mkdir --parents "${cfg.persist}/rootfs/etc"
          [ -f "/etc/shadow" ] && cp /etc/shadow ${pShadow}
          [ -f "${pShadow}" ] && cp ${pShadow} /etc/shadow
        '';

        users.deps = ["etc_shadow"];
      };

      systemd.services."etc_shadow_persistence" = {
        enable = true;
        description = "Persist /etc/shadow on shutdown.";
        wantedBy = ["multi-user.target"];
        path = [pkgs.util-linux];
        unitConfig.defaultDependencies = true;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          # Service is stopped before shutdown
          ExecStop = pkgs.writeShellScript "persist_etc_shadow" ''
            mkdir --parents "${cfg.persist}/rootfs/etc"
            cp /etc/shadow ${pShadow}
          '';
        };
      };
    })
    # Program configuration
    {
      services.ollama.home = "${cfg.persist}/ollama";
      sops.age.keyFile = "${cfg.persist}/sops-key.txt";

      system.activationScripts = {
        openssh_dir.text = "mkdir --parents ${cfg.persist}/ssh";
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
    }
  ]);
}
