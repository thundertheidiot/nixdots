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

# set from infra by the tofu wrapper
variable "vps_v4" {
  type = string
}

variable "vps_v6" {
  type = string
}

locals {
  domain = "meowcloud.net"
  # explicit records keep the wildcard from catching their _acme-challenge names
  # nya is the internal zone, home.idm is the proxied home kanidm
  names = {
    apex     = ""
    nya      = "nya"
    idm      = "idm"
    home_idm = "home.idm"
    oauth    = "oauth"
  }
  addresses = {
    a    = var.vps_v4
    aaaa = var.vps_v6
  }
  records = {
    for pair in setproduct(keys(local.names), keys(local.addresses)) :
    "${pair[0]}_${pair[1]}" => {
      subdomain = local.names[pair[0]]
      type      = upper(pair[1])
      content   = local.addresses[pair[1]]
    }
  }
}

resource "porkbun_dns_record" "vps" {
  for_each  = local.records
  domain    = local.domain
  subdomain = each.value.subdomain
  type      = each.value.type
  content   = each.value.content
}

resource "porkbun_dns_record" "wildcard" {
  domain    = local.domain
  subdomain = "*"
  type      = "CNAME"
  content   = local.domain
}
