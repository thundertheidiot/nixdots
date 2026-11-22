terraform {
  required_providers {
    porkbun = {
      source  = "marcfrederick/porkbun"
      version = "~> 1.3"
    }
  }
}

# PORKBUN_API_KEY and PORKBUN_SECRET_API_KEY come from infra/porkbun.env
provider "porkbun" {}

locals {
  domain = "meowcloud.net"
  vps4   = "95.216.151.56"
  vps6   = "2a01:4f9:c010:9973::1"
}

resource "porkbun_dns_record" "apex_a" {
  domain    = local.domain
  subdomain = ""
  type      = "A"
  content   = local.vps4
}

resource "porkbun_dns_record" "apex_aaaa" {
  domain    = local.domain
  subdomain = ""
  type      = "AAAA"
  content   = local.vps6
}

resource "porkbun_dns_record" "wildcard" {
  domain    = local.domain
  subdomain = "*"
  type      = "CNAME"
  content   = local.domain
}

# landing page, also stops the wildcard from catching *.nya, which breaks dns-01 for the internal cert
resource "porkbun_dns_record" "nya_a" {
  domain    = local.domain
  subdomain = "nya"
  type      = "A"
  content   = local.vps4
}

resource "porkbun_dns_record" "nya_aaaa" {
  domain    = local.domain
  subdomain = "nya"
  type      = "AAAA"
  content   = local.vps6
}

# kanidm on the vps, also stops the wildcard from catching *.idm, which breaks dns-01 for the kanidm certs
resource "porkbun_dns_record" "idm_a" {
  domain    = local.domain
  subdomain = "idm"
  type      = "A"
  content   = local.vps4
}

resource "porkbun_dns_record" "idm_aaaa" {
  domain    = local.domain
  subdomain = "idm"
  type      = "AAAA"
  content   = local.vps6
}

# home kanidm, proxied by the vps, lan and wireguard clients get server2 from the internal dns
resource "porkbun_dns_record" "home_idm_a" {
  domain    = local.domain
  subdomain = "home.idm"
  type      = "A"
  content   = local.vps4
}

resource "porkbun_dns_record" "home_idm_aaaa" {
  domain    = local.domain
  subdomain = "home.idm"
  type      = "AAAA"
  content   = local.vps6
}
