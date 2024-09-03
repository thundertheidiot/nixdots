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
    };
  };

  config = lib.mkIf cfg.enable (let
    mkPersistMount = loc: {
      device = "${cfg.persist}/${loc}";
      fsType = "none";
      options = ["bind"];
      depends = "/${cfg.persist}";
    };

    mkMount = path: let
      inherit (builtins) isString isAttrs;

      mkMount' = {
        path,
        persistPath ? "${cfg.persist}/${path}",
        permissions ? "1777",
        user ? "root",
        group ? "root",
      }: {
        inherit persistPath path permissions user group;
      };
    in
      if (builtins.isString path)
      then mkMount' {inherit path;}
      else if (builtins.isAttrs path)
      then mkMount' path
      else throw "Path provided to impermanence module is not a string or an attrset.";

    mkMounts = list: map (p: mkMount p) list;

    persistMounts = paths': let
      inherit (builtins) listToAttrs;
      inherit (lib.strings) concatStringsSep;

      paths = map (p: mkMount p) paths';
    in {
      systemd.services = listToAttrs (map (p:
        with p; {
          name = "persist-${path}";
          enable = true;
          value = {
            description = "Bind mount ${path}.";
            wantedBy = ["local-fs.target"];
            before = ["local-fs.target"];
            path = [pkgs.util-linux];
            unitConfig.defaultDependencies = false;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              ExecStart = pkgs.writeShellScript "mount_${path}" ''
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

      systemd.tmpfiles.rules =
        (map (p: with p; "d ${path} ${permissions} ${user} ${group} -") paths)
        ++ (map (p: with p; "d ${persistPath} ${permissions} ${user} ${group} -") paths);
    };

    environmentEtcSource = loc: {
      source = "${cfg.persist}/etc/${loc}";
    };
  in
    (persistMounts [
      {
        path = "/var/log";
        permissions = "644";
      }
      "/root/.cache/nix"
      "/etc/NetworkManager/system-connections"
    ])
    // {
      system.activationScripts = {
        openssh_dir.text = "mkdir --parents ${cfg.persist}/etc/ssh";
      };

      environment.etc = builtins.listToAttrs (builtins.map (loc: {
        name = loc;
        value = environmentEtcSource loc;
      }) ["machine-id" "ssh/ssh_host_rsa_key" "ssh/ssh_host_rsa_key.pub" "ssh/ssh_host_ed25519_key" "ssh/ssh_host_ed25519_key.pub"]);
    });
}
