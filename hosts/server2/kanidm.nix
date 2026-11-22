{ ... }: {
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

      groups.admins.members = [ "thunder" ];
      groups.users.members = [
        "pikkukorppu"
        "thunder"
      ];
      groups.unprivileged = { };
    };
  };
}
