{
  fetchFromGitHub,
  buildHomeAssistantComponent,
  home-assistant,
}:
buildHomeAssistantComponent rec {
  owner = "greghesp";
  domain = "bambu_lab";
  version = "2.2.9";

  src = fetchFromGitHub {
    inherit owner;
    repo = "ha-bambulab";
    rev = "v${version}";
    sha256 = "sha256-DJsIB5wFEGF6myTfHblJzIvS+zhGNLbB5j7zSrodP6s=";
  };

  propagatedBuildInputs = with home-assistant.python.pkgs; [
    beautifulsoup4
    paho-mqtt
  ];
}
