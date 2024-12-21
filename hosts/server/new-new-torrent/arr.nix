{lib, ...}: {
  nixpkgs.config.permittedInsecurePackages = [
    "dotnet-sdk-6.0.428"
    "aspnetcore-runtime-6.0.36"
  ];

  vpnNamespaces."airvpn".portMappings = [
    {
      # Radarr
      from = 7878;
      to = 7878;
      protocol = "tcp";
    }
    {
      # Sonarr
      from = 8989;
      to = 8989;
      protocol = "tcp";
    }
    {
      # Lidarr
      from = 8686;
      to = 8686;
      protocol = "tcp";
    }
    {
      # Bazarr
      from = 6767;
      to = 6767;
      protocol = "tcp";
    }
    {
      # Prowlarr
      from = 9696;
      to = 9696;
      protocol = "tcp";
    }
  ];

  meow.impermanence.directories = [
    {
      path = "/var/lib/radarr";
      persistPath = "/persist/media_stack_data/radarr";
      permissions = "777";
      # user = "radarr";
      # group = "users";
    }
    {
      path = "/var/lib/sonarr";
      persistPath = "/persist/media_stack_data/sonarr";
      permissions = "777";
      # user = "sonarr";
      # group = "users";
    }
    {
      path = "/var/lib/lidarr";
      persistPath = "/persist/media_stack_data/lidarr";
      permissions = "777";
      # user = "lidarr";
      # group = "users";
    }
    {
      path = "/var/lib/bazarr";
      persistPath = "/persist/media_stack_data/bazarr";
      user = "bazarr";
      group = "bazarr";
    }
    {
      path = "/var/lib/prowlarr";
      persistPath = "/persist/media_stack_data/prowlarr";
      permissions = "777";
      # user = "prowlarr";
      # group = "users";
    }
  ];

  # Radarr
  systemd.services.radarr.vpnConfinement = {
    enable = true;
    vpnNamespace = "airvpn";
  };
  services.radarr = {
    enable = true;
  };

  # Sonarr
  systemd.services.sonarr.vpnConfinement = {
    enable = true;
    vpnNamespace = "airvpn";
  };
  services.sonarr = {
    enable = true;
  };

  # Lidarr
  systemd.services.lidarr.vpnConfinement = {
    enable = true;
    vpnNamespace = "airvpn";
  };
  services.lidarr = {
    enable = true;
  };

  # Bazarr
  systemd.services.bazarr.vpnConfinement = {
    enable = true;
    vpnNamespace = "airvpn";
  };
  services.bazarr = {
    enable = true;
  };

  # Prowlarr
  users.groups.prowlarr = {};
  users.users.prowlarr = {
    group = "prowlarr";
    home = "/var/lib/prowlarr";
    isSystemUser = true;
  };

  systemd.services.prowlarr = {
    vpnConfinement = {
      enable = true;
      vpnNamespace = "airvpn";
    };

    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "prowlarr";
      Group = "prowlarr";
    };
  };

  services.prowlarr = {
    enable = true;
  };
}
