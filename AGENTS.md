# Basics

Don't overcomplicate! This is a personal repo, you don't need to handle every edgecase, handling what's present is good enough.
Don't create integration tests. Keep comments super minimal, never write full senteces with uppercase letters and periods. NEVER EVER TOUCH THE README.

# Style guide

Import library and builtins functions at the top in a let in block, as if it's an import statement. Like this:

```nix
{lib, config, ...}:
let
  inherit (lib) mkIf;
  inherit (builtins) toJSON;
in {
  config = mkIf true {
    option = toJSON { key = "value"; };
  };
}
```

Use lib.getExe or getExe' to get package binaries, instead of hardcoding `${pkgs.package}/bin/package` into scripts.

Remember to use the helper functions in `flake/lib`. Especially the option ones (`flake/lib/option.nix`) should always be preferred.

The top `let in` block shouldn't get too big, it should just be an import and shared section, don't be afraid to create more let in blocks further down, things used only once should be placed in a block right before they're used for simplicity.

For readability, large modules with many sections should be split up into readable chunks with lib.mkMerge, each chunk being a self contained unit of code.

Each module **must** have a `config` block at the top level. If the module imports nothing, put `{...}:` at the top instead of a `_:`. The order of the module content should be options, imports, config.

# Infra

The host `uwu` is not in use right now, don't worry about it's configuration too much, there's a bunch of old options there. It's fine if it can't evaluate.

These conditions should dictate any planning and changes on the server infrastructure:
1. Everything on server2 must remain accessible without access to the internet or VPS.
2. Everything on the vps must remain accessible without access to server2.
