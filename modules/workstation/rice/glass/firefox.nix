{
  lib,
  mlib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkForce;
  inherit (mlib) homeModule;
  inherit (pkgs) replaceVars;

  cfg = config.meow.rice;
in {
  config = mkIf (cfg == "glass") (homeModule {
    programs.firefox.profiles."nix-managed" = {
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      };

      # userChrome = let
      #   colors = config.meow.workstation.theme.palette.withHashtag;

      #   inherit (colors) base01;
      # in ''
      #   :root {
      #     --bg: alpha(${base01}, 0.95);
      #   }

      #   #navigator-toolbox,
      #   #TabsToolbar,
      #   #nav-bar,
      #   #PersonalToolbar,
      #   #toolbar-menubar,
      #   #titlebar {
      #     background-color: var(--semi-bg) !important;
      #     /* Optional blur (comment out if you don’t want it) */
      #     backdrop-filter: blur(8px) saturate(1.1);
      #   }

      #   /* Native sidebar container + header (works with the built-in “Tabs” sidebar) */
      #   #sidebar-box,
      #   #sidebar,
      #   #sidebar-header {
      #     background-color: var(--semi-bg) !important;
      #   }

      #   /* Make tab backgrounds themselves transparent so they sit on the semi‑transparent bar */
      #   .tabbrowser-tab .tab-background {
      #     background: transparent !important;
      #     border: none !important;
      #   }

      #   /* Optional: subtle separators for readability */
      #   .tabbrowser-tab:not([selected]) {
      #     border-inline-end: 1px solid color-mix(in srgb, currentColor 12%, transparent) !important;
      #   }

      #   /* Optional: remove thin border under the toolbars */
      #   #navigator-toolbox::after {
      #     border: none !important;
      #   }
      # '';

      # userContent = ''
      #     @-moz-document url-prefix("chrome://browser/content/sidebars/") {
      #     :root, html, body {
      #       background: transparent !important;
      #     }

      #     /* Common containers seen in Tabs sidebar builds; keep them transparent */
      #     #vertical-tabs,
      #     #vt-root,
      #     .vt-sidebar,
      #     #tablist,
      #     .tablist,
      #     .tablist-scrollbox,
      #     .tablist-outer,
      #     .tab-row,
      #     .tabcontainer,
      #     .tabbrowser-arrowscrollbox {
      #       background: transparent !important;
      #     }
      #   }

      #   /* Fallbacks for other internal sidebar documents that may be used on some channels */
      #   @-moz-document url("chrome://browser/content/syncedtabs/sidebar.xhtml"),
      #                  url-prefix("chrome://browser/content/tabbrowser/") {
      #     :root, html, body { background: transparent !important; }
      #   }
      # '';
    };
  });
}
