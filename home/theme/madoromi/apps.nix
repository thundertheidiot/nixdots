{
  config,
  lib,
  mlib,
  theme,
  ...
}:
let
  inherit (lib)
    concatStringsSep
    mapAttrs
    mapAttrsToList
    mkIf
    mkMerge
    removePrefix
    ;
  inherit (mlib) hexToRgb;
  palette = theme.palette;
  roles = palette.roles;
in
{
  config = mkIf (theme.family == "madoromi") (mkMerge [
    (mkIf config.programs.alacritty.enable {
      programs.alacritty.settings.colors = {
        primary = {
          background = roles.background;
          foreground = roles.foreground;
        };
        cursor = {
          cursor = roles.cursor;
          text = roles.cursorText;
        };
        selection = {
          background = roles.selection;
          text = roles.selectionText;
        };
        inherit (palette.noctalia.dark.terminal) normal bright;
      };
    })
    (mkIf config.programs.fish.enable {
      xdg.configFile."fish/themes/madoromi.theme".text =
        let
          color = role: removePrefix "#" roles.${role};
          themeColors = {
            fish_color_normal = color "foreground";
            fish_color_command = color "function";
            fish_color_keyword = color "keyword";
            fish_color_quote = color "string";
            fish_color_redirection = color "link";
            fish_color_end = color "accent";
            fish_color_error = color "error";
            fish_color_param = color "foreground";
            fish_color_comment = color "comment";
            fish_color_operator = color "link";
            fish_color_escape = color "accent";
            fish_color_autosuggestion = color "subtle";
            fish_color_search_match = "${color "foreground"} --background=${color "surface"}";
            fish_color_selection = "${color "foreground"} --background=${color "selection"}";
            fish_color_cancel = "--reverse";
            fish_pager_color_progress = color "accent";
            fish_pager_color_prefix = color "accent";
            fish_pager_color_completion = color "foreground";
            fish_pager_color_description = color "muted";
            fish_pager_color_selected_background = "--background=${color "selection"}";
          };
        in
        concatStringsSep "\n" (mapAttrsToList (name: value: "${name} ${value}") themeColors);
      programs.fish.shellInit = ''
        fish_config theme choose madoromi
      '';
    })
    (mkIf config.programs.mpv.enable {
      programs.mpv.config = {
        osd-color = "#FF${removePrefix "#" roles.foreground}";
        osd-border-color = "#FF${removePrefix "#" roles.background}";
        osd-back-color = "#CC${removePrefix "#" roles.surface}";
      };
    })
    (mkIf config.programs.firefox.enable {
      programs.firefox.profiles.nix-managed.extensions.settings."FirefoxColor@mozilla.com" = {
        force = true;
        settings = {
          firstRunDone = true;
          theme = {
            title = "Madoromi ${theme.variant}";
            images = {
              additional_backgrounds = [ ];
              custom_backgrounds = [ ];
            };
            colors = mapAttrs (_: hexToRgb) {
              frame = roles.outer;
              frame_inactive = roles.outer;
              tab_background_text = roles.foreground;
              tab_selected = roles.background;
              tab_text = roles.foreground;
              tab_line = roles.accent;
              tab_loading = roles.accent;
              toolbar = roles.background;
              toolbar_text = roles.foreground;
              toolbar_field = roles.panel;
              toolbar_field_text = roles.foreground;
              toolbar_field_focus = roles.surface;
              toolbar_field_border = roles.border;
              toolbar_field_border_focus = roles.accent;
              toolbar_field_highlight = roles.selection;
              toolbar_field_highlight_text = roles.selectionText;
              toolbar_bottom_separator = roles.border;
              popup = roles.panel;
              popup_text = roles.foreground;
              popup_border = roles.border;
              popup_highlight = roles.selection;
              popup_highlight_text = roles.selectionText;
              icons = roles.foreground;
              icons_attention = roles.accent;
              ntp_background = roles.background;
              ntp_text = roles.foreground;
              sidebar = roles.panel;
              sidebar_text = roles.foreground;
              sidebar_border = roles.border;
              sidebar_highlight = roles.selection;
              sidebar_highlight_text = roles.selectionText;
              button_background_active = roles.hover;
            };
          };
        };
      };
    })
    (mkIf config.programs.opencode.enable {
      programs.opencode = {
        themes.madoromi = {
          "$schema" = "https://opencode.ai/theme.json";
          theme = {
            primary = roles.accent;
            secondary = roles.info;
            accent = roles.accent;
            error = roles.error;
            warning = roles.warning;
            success = roles.success;
            info = roles.info;
            text = roles.foreground;
            textMuted = roles.muted;
            selectedListItemText = roles.foreground;
            background = roles.background;
            backgroundPanel = roles.panel;
            backgroundElement = roles.surface;
            border = roles.border;
            borderActive = roles.accent;
            borderSubtle = roles.surface;
            diffAdded = roles.success;
            diffRemoved = roles.error;
            diffContext = roles.muted;
            diffHunkHeader = roles.accent;
            diffHighlightAdded = roles.success;
            diffHighlightRemoved = roles.error;
            diffAddedBg = roles.panel;
            diffRemovedBg = roles.panel;
            diffContextBg = roles.background;
            diffLineNumber = roles.subtle;
            diffAddedLineNumberBg = roles.panel;
            diffRemovedLineNumberBg = roles.panel;
            markdownText = roles.foreground;
            markdownHeading = roles.heading;
            markdownLink = roles.link;
            markdownLinkText = roles.link;
            markdownCode = roles.string;
            markdownBlockQuote = roles.comment;
            markdownEmph = roles.foreground;
            markdownStrong = roles.foreground;
            markdownHorizontalRule = roles.border;
            markdownListItem = roles.number;
            markdownListEnumeration = roles.number;
            markdownImage = roles.accent;
            markdownImageText = roles.foreground;
            markdownCodeBlock = roles.foreground;
            syntaxComment = roles.comment;
            syntaxKeyword = roles.keyword;
            syntaxFunction = roles.function;
            syntaxVariable = roles.foreground;
            syntaxString = roles.string;
            syntaxNumber = roles.number;
            syntaxType = roles.type;
            syntaxOperator = roles.operator;
            syntaxPunctuation = roles.punctuation;
          };
        };
        tui.theme = "madoromi";
      };
    })
  ]);
}
