{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "1.22.2";

  src = fetchurl {
    url = "https://github.com/SableClient/Sable/releases/download/v${finalAttrs.version}/Sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-yqbCJUBetGr7HP/rC9vDIbXfA8mE8Nrtki+FUsa0Dhk=";
  };
})
