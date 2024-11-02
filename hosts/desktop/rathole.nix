{pkgs, ...}: {
  config = {
    meow.sops.enableSecrets = ["desk_rathole"];
    meow.sops.secrets."desk_rathole" = {
      path = "/etc/rathole.toml";
      mode = "0644";
    };

    systemd.services.rathole = {
      description = "Rathole";
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        ExecStart = pkgs.writeShellScript "rathole" ''
          ${pkgs.rathole}/bin/rathole /etc/rathole.toml
        '';
      };
    };
  };
}
