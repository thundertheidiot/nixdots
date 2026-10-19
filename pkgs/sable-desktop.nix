{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.0.3";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-nxD3y+e70+Ato6dKKWef9ywiq6/aCSBOujoODiAB9G8=";
  };
})
