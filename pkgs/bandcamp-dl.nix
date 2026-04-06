{
  fetchFromGitHub,
  fetchPypi,
  python313Packages,
}: let
  demjson3 = python313Packages.buildPythonPackage (finalAttrs: {
    pname = "demjson3";
    version = "3.0.6";
    src = fetchPypi {
      pname = "demjson3";
      version = finalAttrs.version;
      hash = "sha256-N8g7DG6wjSXe/IjfCipIddWKeAmpZQvW7uev2AU826w=";
    };
    pyproject = true;
    build-system = with python313Packages; [setuptools];
  });

  unicode-slugify = python313Packages.buildPythonPackage (finalAttrs: {
    pname = "unicode-slugify";
    version = "a18826f";
    src = fetchFromGitHub {
      owner = "dnicolson";
      repo = "unicode-slugify";
      rev = finalAttrs.version;
      hash = "sha256-kxyJaHxd/cMHiycfQHJqSsecT+eMkkRqO+OMSzbtjmE=";
    };
    pyproject = true;
    build-system = with python313Packages; [setuptools];
    dependencies = with python313Packages; [
      six
      unidecode
    ];
  });
in
  python313Packages.buildPythonPackage (finalAttrs: {
    pname = "bandcamp-dl";
    version = "d23867b";
    pyproject = true;

    src = fetchFromGitHub {
      owner = "Evolution0";
      repo = "bandcamp-dl";
      rev = "${finalAttrs.version}";
      hash = "sha256-NfsW02zp2jkcPrZwys5PmYGR/fPseSH8+fvXRlGVE+Q=";
    };

    build-system = with python313Packages; [setuptools];

    dependencies = with python313Packages; [
      beautifulsoup4
      demjson3
      mutagen
      requests
      unicode-slugify
      urllib3
      toml
    ];
  })
