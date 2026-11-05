{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.3.4";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-x86_64.AppImage";
    hash = "sha256-tlDnIoWEPCabQzKhLj5HAe9fgNmRqV72sNeclq0B7rQ=";
  };
})
