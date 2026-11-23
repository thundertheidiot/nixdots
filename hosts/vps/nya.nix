{
  config,
  infra,
  pkgs,
  ...
}:
let
  nya = infra.dns.internal;
in
{
  config = {
    services.nginx.virtualHosts.${nya} = {
      forceSSL = true;
      useACMEHost = nya;
      root = pkgs.writeTextDir "index.html" ''
        <!doctype html>
        <meta charset="utf-8">
        <title>nya</title>
        <style>html,body{height:100%;margin:0;display:grid;place-items:center;font-size:30vmin}</style>
        🐱
      '';
    };

    # the zone is local on the vpn, point the bare name back here
    services.dnsmasq.settings.host-record = [ "${nya},${config.meow.dns.addresses.vps}" ];
  };
}
