let
  agenix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHAEqhBA11QeQTsbsTtOu14+n4adDQoCUqrgL69dbUMy";
  tv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG0ozgHXS8XFjkUh9D5oz+pOfAedB65TSBZfPMEwi869";

  keys = [agenix];
  tvKeys = [tv agenix];
in {
  "secrets/kodi_youtube_api_keys.age".publicKeys = tvKeys;
  "secrets/kodi_jellyfin_data.age".publicKeys = tvKeys;
}
