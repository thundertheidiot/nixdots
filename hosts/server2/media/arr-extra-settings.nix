let
  settings = {
    collections.remotepathmapping = [
      {
        host = "192.168.15.1";
        remotePath = "/downloads/";
        localPath = "/media/downloads/torrents/";
      }
    ];
  };
in
{
  radarr = settings;
  sonarr = settings;
}
