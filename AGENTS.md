# Basics

Don't overcomplicate! This is a personal repo, you don't need to handle every edgecase, handling what's present is good enough.
Don't create integration tests.

# Style guide

Import library functions at the top in a let in block, as if it's an import statement. Like this:

```nix
{lib, config, ...}:
let
  inherit (lib) mkIf
in {
  config = mkIf true {
    option = "value";
  };
}
```

Remember to use the helper functions in `flake/lib`. Especially the option ones (`flake/lib/option.nix`) should always be preferred.

The top `let in` block shouldn't get too big, it should just be an import and shared section, don't be afraid to create more let in blocks further down, things used only once should be placed in a block right before they're used for simplicity.

For readability, large modules with many sections should be split up into readable chunks with lib.mkMerge, each chunk being a self contained unit of code.
