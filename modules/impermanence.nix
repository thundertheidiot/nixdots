{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.meow.impermanence;
  persistedShadow = "${cfg.persist}/rootfs/etc/shadow";

  inherit (lib)
    mkIf
    mkMerge
    isString
    escapeShellArg
    optional
    any
    ;
  inherit (lib.options) mkOption;
  inherit (lib.lists) flatten;
  inherit (lib.strings)
    concatStringsSep
    replaceStrings
    optionalString
    hasPrefix
    ;
  inherit (lib.attrsets) filterAttrs mapAttrsToList listToAttrs;
  inherit (lib.types)
    listOf
    bool
    str
    attrs
    either
    ;
in
{
  options = {
    meow.impermanence = {
      enable = mkOption {
        type = bool;
        default = true;
        description = "Enable impermanence";
      };

      persist = mkOption {
        type = str;
        default = "/nix/persist";
        description = "Directory to use for persistance of files.";
      };

      directories = mkOption {
        type = listOf (either attrs str);
        default = [ ];
        apply =
          let
            mkDir' =
              {
                path,
                persistPath ? "${cfg.persist}/rootfs/${path}",
                permissions ? "0755",
                user ? "root",
                group ? "root",
                wantedBy ? [ ],
                before ? [ ],
              }:
              {
                inherit
                  path
                  persistPath
                  permissions
                  user
                  group
                  wantedBy
                  before
                  ;
              };

            mkDir = dir: if isString dir then mkDir' { path = dir; } else mkDir' dir;
          in
          list: map mkDir list;
        description = "Directories to persist across reboots.";
      };

      files = mkOption {
        type = listOf (either attrs str);
        default = [ ];
        apply =
          let
            mkFile' =
              {
                path,
                persistPath ? "${cfg.persist}/rootfs/${path}",
                permissions ? "0644",
                user ? "root",
                group ? "root",
                wantedBy ? [ ],
                before ? [ ],
              }:
              {
                inherit
                  path
                  persistPath
                  permissions
                  user
                  group
                  wantedBy
                  before
                  ;
              };

            mkFile = file: if isString file then mkFile' { path = file; } else mkFile' file;
          in
          list: map mkFile list;
        description = "Files to persist across reboots.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      meow.impermanence.directories = [
        {
          path = "/var/log";
          permissions = "711";
        }
        {
          path = "/root/.cache/nix";
          permissions = "0700";
        }
        "/var/lib/systemd"
        {
          path = "/var/lib/fprint";
          permissions = "0700";
        }
        {
          path = "/etc/NetworkManager/system-connections";
          permissions = "0700";
        }
        "/var/cache/man"
        "/var/lib/fwupd"
        "/var/cache/fwupd"
        {
          path = "/var/db/sudo";
          permissions = "0700";
        }
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
      ];

      meow.impermanence.files = [
        "/etc/localtime"
        "/etc/machine-id"
      ];
    })

    # Create and mount directories
    (mkIf cfg.enable {
      systemd.mounts = map (
        dir: with dir; {
          where = path;
          what = persistPath;
          type = "none";
          options = "bind,X-fstrim.notrim,x-gvfs-hidden";

          requires = [ "persistence-directories.service" ];
          after = [ "persistence-directories.service" ];
          unitConfig.RequiresMountsFor = [ persistPath ];
          before = [ "local-fs.target" ] ++ before;
          wantedBy = [ "local-fs.target" ] ++ wantedBy;
        }
      ) cfg.directories;

      # Names may not exist yet. Create mount sources without changing existing
      # ownership; tmpfiles applies named ownership after accounts and mounts.
      systemd.services.persistence-directories = {
        unitConfig = {
          DefaultDependencies = false;
          RequiresMountsFor = map (dir: dir.persistPath) cfg.directories;
        };
        after = [ "systemd-remount-fs.service" ];
        before = [ "local-fs.target" ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        script = concatStringsSep "\n" (
          map (dir: ''
            ${optionalString (hasPrefix "/var/lib/private/" dir.path) "install -d -m 0700 /var/lib/private"}
            mkdir -p -- ${escapeShellArg dir.persistPath} ${escapeShellArg dir.path}
            chmod ${escapeShellArg dir.permissions} -- ${escapeShellArg dir.persistPath}
          '') cfg.directories
        );
      };

      systemd.tmpfiles.rules =
        optional (any (
          dir: hasPrefix "/var/lib/private/" dir.path
        ) cfg.directories) "d /var/lib/private 0700 root root - -"
        ++ flatten (
          map (
            dir:
            with dir;
            let
              # systemd owns private StateDirectory storage, including dynamic UIDs.
              owner = if hasPrefix "/var/lib/private/" path then "- -" else "${user} ${group}";
            in
            [
              "d ${persistPath} ${permissions} ${owner} - -"
              "d ${path} ${permissions} ${owner} - -"
            ]
          ) cfg.directories
        );
    })

    # Create and mount files
    (mkIf cfg.enable {
      boot.postBootCommands = concatStringsSep " " (
        map (
          file: with file; ''
            if [ -e "${persistPath}" ] || [ -L "${persistPath}" ]; then
              cp -P -- ${escapeShellArg persistPath} ${escapeShellArg path}
              chown -h ${escapeShellArg "${user}:${group}"} -- ${escapeShellArg path}
              if [ ! -L ${escapeShellArg path} ]; then
                chmod ${escapeShellArg permissions} -- ${escapeShellArg path}
              fi
            fi
          ''
        ) cfg.files
      );

      systemd.services = listToAttrs (
        map (
          file:
          with file;
          let
            name = "persist-${replaceStrings [ "/" ] [ "_" ] path}";
          in
          {
            inherit name;
            value = {
              wantedBy = [ "default.target" ] ++ wantedBy;
              inherit before;
              path = [ pkgs.util-linux ];
              unitConfig.DefaultDependencies = true;
              unitConfig.RequiresMountsFor = [ persistPath ];
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
                # Service is stopped before shutdown
                ExecStop = pkgs.writeShellScript name ''
                  umask 077
                  mkdir --parents -- ${escapeShellArg (dirOf persistPath)}
                  cp -P --preserve=mode,ownership -- ${escapeShellArg path} ${escapeShellArg persistPath}
                '';
              };
            };
          }
        ) cfg.files
      );
    })

    ### fixes/hacks

    # Account allocation state must be available before user/group setup.
    (mkIf cfg.enable (
      let
        persistedState = "${cfg.persist}/rootfs/var/lib/nixos";
      in
      {
        system.activationScripts = {
          persist-nixos-state = {
            deps = [ "specialfs" ];
            text = ''
              if ! ${pkgs.util-linux}/bin/mountpoint -q /var/lib/nixos; then
                mkdir -p -- ${escapeShellArg persistedState}
                # copy state when switching an existing installation
                if [ -d /var/lib/nixos ]; then
                  cp -a /var/lib/nixos/. ${escapeShellArg persistedState}/
                fi
                mkdir -p /var/lib/nixos
                ${pkgs.util-linux}/bin/mount --bind ${escapeShellArg persistedState} /var/lib/nixos
              fi
            '';
          };
          users.deps = [ "persist-nixos-state" ];
        };
      }
    ))

    # home directories
    (mkIf cfg.enable {
      systemd.tmpfiles.rules = mapAttrsToList (
        name: user: "d ${user.home} 0700 ${name} ${user.group} - -"
      ) (filterAttrs (_name: attrs: attrs.createHome) config.users.users);
    })

    # /etc/shadow (passwords)
    # cannot be handled through files, must run before user setup
    (mkIf cfg.enable {
      system.activationScripts = {
        restore-persistent-shadow = {
          deps = [ "specialfs" ];
          text = ''
            # Restore on boot, but do not overwrite live passwords during a switch.
            if [ ! -e /etc/shadow ] && [ -f ${escapeShellArg persistedShadow} ]; then
              install -m 0600 -o 0 -g 0 ${escapeShellArg persistedShadow} /etc/shadow
            fi
          '';
        };
        users.deps = [ "restore-persistent-shadow" ];
      };
      systemd.services.etc_shadow_persistence = {
        description = "Persist /etc/shadow on shutdown.";
        wantedBy = [ "multi-user.target" ];
        unitConfig.RequiresMountsFor = [ cfg.persist ];
        script = "true";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStop = pkgs.writeShellScript "persist_etc_shadow" ''
            if [ -f /etc/shadow ]; then
              umask 077
              mkdir --parents -- ${escapeShellArg (dirOf persistedShadow)}
              install -m 0600 -o 0 -g 0 /etc/shadow ${escapeShellArg "${persistedShadow}.new"}
              mv -f -- ${escapeShellArg "${persistedShadow}.new"} ${escapeShellArg persistedShadow}
            fi
          '';
        };
      };
    })

    # Program configuration
    (mkIf cfg.enable {
      sops.age.keyFile = "${cfg.persist}/sops-key.txt";

      systemd.tmpfiles.rules = [ "d ${cfg.persist}/ssh 755 root root - -" ];

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
    })
  ];
}
