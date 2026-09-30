{
  inputs,
  lib,
  server,
  ...
}:
let
  inherit (lib) unique filter hasSuffix;

  domains = unique (
    filter (
      domain: !hasSuffix ".local" domain
    ) inputs.self.nixosConfigurations.server2.config.server.domains
  );
in
{
  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
    settings = {
      interface = "wg0";
      listen-address = "10.100.0.1";
      bind-dynamic = true;
      no-resolv = true;
      cache-size = 1000;
      local = [
        "/home/"
        "/server/"
        "/local/"
        "/desktop/"
      ];
      address = map (domain: "/${domain}/${server.homeServer2}") domains;
      server = [ "1.1.1.1" ];
    };
  };
}
