# MIT License
# Copyright (c) 2017-2023 Home Manager contributors
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
{
  runCommand,
  mozlz4a,
  openssl,
  lib,
  path,
  name,
  engines,
  default,
  privateDefault,
  order,
}: let
  inherit (lib) imap;
  inherit (lib.attrsets) mapAttrs mapAttrs' genAttrs optionalAttrs attrValues;
  inherit (builtins) replaceStrings;
  settings = {
    version = 6;
    engines = let
      # Map of nice field names to internal field names.
      # This is intended to be exhaustive and should be
      # updated at every version bump.
      internalFieldNames =
        (genAttrs [
          "name"
          "isAppProvided"
          "loadPath"
          "hasPreferredIcon"
          "updateInterval"
          "updateURL"
          "iconUpdateURL"
          "iconURL"
          "iconMapObj"
          "metaData"
          "orderHint"
          "definedAliases"
          "urls"
        ] (name: "_${name}"))
        // {
          searchForm = "__searchForm";
        };

      processCustomEngineInput = input:
        (removeAttrs input ["icon"])
        // optionalAttrs (input ? icon) {
          # Convenience to specify absolute path to icon
          iconURL = "file://${input.icon}";
        }
        // (optionalAttrs (input ? iconUpdateURL) {
            # Convenience to default iconURL to iconUpdateURL so
            # the icon is immediately downloaded from the URL
            iconURL = input.iconURL or input.iconUpdateURL;
          }
          // {
            # Required for custom engine configurations, loadPaths
            # are unique identifiers that are generally formatted
            # like: [source]/path/to/engine.xml
            loadPath = ''
              [home-manager]/programs.firefox.profiles.${name}.search.engines."${
                replaceStrings ["\\"] ["\\\\"] input.name
              }"'';
          });

      processEngineInput = name: input: let
        requiredInput = {
          inherit name;
          isAppProvided =
            input.isAppProvided or removeAttrs input
            ["metaData"]
            == {};
          metaData = input.metaData or {};
        };
      in
        if requiredInput.isAppProvided
        then requiredInput
        else processCustomEngineInput (input // requiredInput);

      buildEngineConfig = name: input:
        mapAttrs' (name: value: {
          name = internalFieldNames.${name} or name;
          inherit value;
        }) (processEngineInput name input);

      sortEngineConfigs = configs: let
        buildEngineConfigWithOrder = order: name: let
          config =
            configs.${name}
            or {
              _name = name;
              _isAppProvided = true;
              _metaData = {};
            };
        in
          config
          // {
            _metaData = config._metaData // {inherit order;};
          };

        engineConfigsWithoutOrder =
          attrValues (removeAttrs configs order);

        sortedEngineConfigs =
          (imap buildEngineConfigWithOrder order)
          ++ engineConfigsWithoutOrder;
      in
        sortedEngineConfigs;

      engineInput =
        engines
        // {
          # Infer profile.search.default as an app provided
          # engine if it's not in profile.search.engines
          ${default} =
            engines.${default} or {};
        }
        // {
          ${privateDefault} =
            engines.${privateDefault} or {};
        };
    in
      sortEngineConfigs (mapAttrs buildEngineConfig engineInput);

    metaData =
      optionalAttrs (default != null) {
        current = default;
        hash = "@hash@";
      }
      // optionalAttrs (privateDefault != null) {
        private = privateDefault;
        privateHash = "@privateHash@";
      }
      // {
        useSavedOrder = order != [];
      };
  };

  # Home Manager doesn't circumvent user consent and isn't acting
  # maliciously. We're modifying the search outside of Firefox, but
  # a claim by Mozilla to remove this would be very anti-user, and
  # is unlikely to be an issue for our use case.
  disclaimer = appName:
    "By modifying this file, I agree that I am doing so "
    + "only within ${appName} itself, using official, user-driven search "
    + "engine selection processes, and in a way which does not circumvent "
    + "user consent. I acknowledge that any attempt to change this file "
    + "from outside of ${appName} is a malicious act, and will be responded "
    + "to accordingly.";

  salt =
    if default != null
    then path + default + disclaimer "Firefox"
    else null;

  privateSalt =
    if privateDefault != null
    then
      path
      + privateDefault
      + disclaimer "Firefox"
    else null;
in
  runCommand "search.json.mozlz4" {
    nativeBuildInputs = [mozlz4a openssl];
    json = builtins.toJSON settings;
    inherit salt privateSalt;
  } ''
    if [[ -n $salt ]]; then
      export hash=$(echo -n "$salt" | openssl dgst -sha256 -binary | base64)
      export privateHash=$(echo -n "$privateSalt" | openssl dgst -sha256 -binary | base64)
      mozlz4a <(substituteStream json search.json.in --subst-var hash --subst-var privateHash) "$out"
    else
      mozlz4a <(echo "$json") "$out"
    fi
  ''
