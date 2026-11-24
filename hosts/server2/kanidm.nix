{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe getExe';
in
{
  config = {
    meow.kanidm.enable = true;

    # provisioning runs only here, replication carries it to the vps
    services.kanidm.provision = {
      enable = true;

      persons.thunder = {
        displayName = "thunder";
        mailAddresses = [ "thundertheidiot@proton.me" ];
      };

      persons.pikkukorppu = {
        displayName = "pikkukorppu";
      };

      persons.delta = {
        displayName = "delta";
      };

      groups.admins.members = [ "thunder" ];
      groups.users.members = [
        "pikkukorppu"
        "thunder"
        "delta"
      ];
      groups.unprivileged = { };
    };

    # continuwuity runs on the vps, its side is in the matrix module
    meow.sops.enableSecrets = [ "kanidm_continuwuity" ];
    services.kanidm.provision.systems.oauth2.continuwuity = {
      displayName = "Matrix";
      originUrl = "https://meowcloud.net/_continuwuity/oidc/complete";
      originLanding = "https://matrix.meowcloud.net/";
      basicSecretFile = config.sops.secrets.kanidm_continuwuity.path;
      # the spn isn't a valid localpart
      preferShortUsername = true;
      scopeMaps.users = [
        "openid"
        "profile"
        "email"
      ];
    };

    # account policy isn't provisionable and migrations can't touch the builtin idm_all_persons
    systemd.services.kanidm-account-policy =
      let
        kanidm = getExe' config.services.kanidm.package "kanidm";
        kanidmd = getExe' config.services.kanidm.package "kanidmd";
        policy = "${kanidm} group account-policy";
      in
      {
        description = "kanidm account policy";
        after = [ "kanidm.service" ];
        requires = [ "kanidm.service" ];
        wantedBy = [ "kanidm.service" ];
        serviceConfig = {
          Type = "oneshot";
          User = "kanidm";
          Group = "kanidm";
          RuntimeDirectory = "kanidm-account-policy";
        };
        environment = {
          KANIDM_URL = config.services.kanidm.provision.instanceUrl;
          KANIDM_ACCEPT_INVALID_CERTS = "true";
          # the cli creates ~/.cache for its tokens no matter what, the kanidm home is /var/empty
          HOME = "/run/kanidm-account-policy";
        };
        script = ''
          KANIDM_PASSWORD=$(${kanidmd} scripting recover-account -c /etc/kanidm/server.toml idm_admin \
            | ${getExe pkgs.jq} -r .output)
          export KANIDM_PASSWORD
          ${kanidm} login -D idm_admin

          # passwords alone for users, admin must have mfa
          ${policy} credential-type-minimum idm_all_persons any -D idm_admin
          ${policy} enable admins -D idm_admin
          ${policy} credential-type-minimum admins mfa -D idm_admin
        '';
      };
  };
}
