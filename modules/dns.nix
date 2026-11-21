{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib)
    filterAttrs
    mapAttrs'
    mapAttrsToList
    mkIf
    nameValuePair
    ;
  inherit (lib.types) str;

  cfg = config.meow.dns;
  inherit (cfg) domain;

  # service name -> host serving it
  records = {
    bazarr = "server2";
    git = "server2";
    homeassistant = "server2";
    homepage = "server2";
    jellyfin = "server2";
    metube = "server2";
    navidrome = "server2";
    prowlarr = "server2";
    radarr = "server2";
    reddit = "server2";
    sonarr = "server2";
    soulbeet = "server2";
    soulseek = "server2";
    syncthing = "server2";
    torrent = "server2";

    books = "vps";
    dash = "vps";
    rss = "vps";
  };
in
{
  options.meow.dns = {
    enable = mkEnOpt "internal dns records and wildcard certificate";
    domain = mkOpt str "nya.meowcloud.net" { };
    server2 = mkOpt str "" { description = "server2 address as seen by this host's dns clients"; };
    vps = mkOpt str "" { description = "vps address as seen by this host's dns clients"; };
  };

  config = mkIf cfg.enable {
    services.dnsmasq.settings = {
      local = [ "/${domain}/" ];
      host-record = mapAttrsToList (name: host: "${name}.${domain},${cfg.${host}}") records;
    };

    sops.secrets.porkbun = {
      sopsFile = ../sops/porkbun.env;
      format = "dotenv";
    };

    security.acme.certs.${domain} = {
      domain = "*.${domain}";
      extraDomainNames = [ domain ];
      dnsProvider = "porkbun";
      # the local dnsmasq shadows the zone
      dnsResolver = "1.1.1.1:53";
      environmentFile = config.sops.secrets.porkbun.path;
    };

    services.nginx.virtualHosts = mapAttrs' (
      name: _: nameValuePair "${name}.${domain}" { useACMEHost = domain; }
    ) (filterAttrs (_: host: host == config.networking.hostName) records);
  };
}
