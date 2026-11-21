_: {
  meow.kanidm.enable = true;

  # provisioning runs only here, replication carries it to the vps
  # oauth2 clients are set up next to the services using them
  services.kanidm.provision = {
    enable = true;

    persons.thunder = {
      displayName = "thunder";
      mailAddresses = [ "thundertheidiot@proton.me" ];
    };

    groups.admins.members = [ "thunder" ];
  };
}
