{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (lib) mkIf;
  inherit (lib.types) bool str attrs;
  inherit (lib.attrsets) mergeAttrsList;
  inherit (mlib) mkOpt mkEnOpt defineJqDeepmerge applyWithJq;
  inherit (builtins) toJSON;
  cfg = config.meow.program.discordConfig;
in {
  options = {
    meow.program.discordConfig = {
      enable = mkOpt bool true {
        description = "Discord";
      };
      settings = mkOpt attrs {} {
        description = "Vesktop settings.json file";
      };
      userSettings = mkOpt attrs {} {
        description = "Vesktop settings/settings.json file";
      };
      plugins = mkOpt attrs {} {
        description = "Vesktop plugin configuration";
      };

      overrideQuickCss = mkEnOpt "Override quickcss instead of adding to it.";
      quickCss = mkOpt str "" {
        description = "Additional quickcss";
      };
    };
  };

  config = mkIf (config.meow.program.discord && cfg.enable) (let
    createFile = {
      name,
      path ? "${config.meow.home.directory}/.config/vesktop/",
      attrs,
    }: let
      file = "${path}/${name}";
    in ''
      if [ ! -f "${file}" ]; then
        mkdir -p ${path}
        echo '{}' > "${file}"
      fi

      ${applyWithJq {
        jq = "${pkgs.jq}/bin/jq";
        args = "--argjson settings '${toJSON attrs}'";
        file = "${file}";
        operation = "${defineJqDeepmerge} deepmerge({}; [., $settings])";
      }}
    '';

    defaultSettings = {
      discordBranch = "stable";
      splashColor = "rgb(205, 214, 244)";
      splashBackground = "rgb(30, 30, 46)";
      staticTitle = true;
      minimizeToTray = false;
      tray = false;
    };

    defaultUserSettings = {
      autoUpdate = true;
      autoUpdateNotification = true;
      useQuickCss = true;
      disableMinSize = true;
      themeLinks = [
        "https://catppuccin.github.io/discord/dist/catppuccin-mocha.theme.css"
      ];
    };

    defaultQuickCss = ''
      /* Hide Nitro gift button */
      button[aria-label="Send a gift"] {
          display: none;
      }
      /* Hide sticker picker button */
      button[aria-label="Open sticker picker"] {
          display: none;
      }
      /* Hide annoying sticker popup window that appears when you type */
      .channelTextArea-1FufC0 > .container-1ZA19X {
          display: none;
      }
    '';

    defaultPlugins = {
      ChatInputButtonAPI.enabled = true;
      CommandsAPI.enabled = true;
      MemberListDecoratorsAPI.enabled = true;
      MessageAccessoriesAPI.enabled = true;
      MessageDecorationsAPI.enabled = true;
      MessageEventsAPI.enabled = true;
      MessageUpdaterAPI.enabled = true;
      UserSettingsAPI.enabled = true;
      WebContextMenus = {
        addBack = true;
        enabled = true;
      };
      Settings = {
        enabled = true;
        settingsLocation = "aboveNitro";
      };
      NoTrack = {
        disableAnalytics = true;
        enabled = true;
      };

      AlwaysAnimate.enabled = true;
      AlwaysTrust = {
        domain = true;
        enabled = true;
        file = true;
      };
      AnonymiseFileNames = {
        anonymiseByDefault = true;
        consistent = "image";
        enabled = true;
        method = 0;
        randomisedLength = 7;
      };
      BetterSettings = {
        disableFade = true;
        eagerLoad = true;
        enabled = true;
        organizeMenu = true;
      };
      BiggerStreamPreview.enabled = true;
      BlurNSFW = {
        blurAmount = 10;
        enabled = true;
      };
      CallTimer = {
        enabled = true;
        format = "stopwatch";
      };
      Decor = {
        agreedToGuidelines = false;
        enabled = true;
      };
      FakeNitro = {
        disableEmbedPermissionCheck = false;
        emojiSize = 48;
        enableEmojiBypass = true;
        enableStickerBypass = true;
        enableStreamQualityBypass = false;
        enabled = true;
        hyperLinkText = "{{NAME}}";
        stickerSize = 160;
        transformCompoundSentence = true;
        transformEmojis = true;
        transformStickers = true;
        useHyperLinks = true;
      };
      FixYoutubeEmbeds.enabled = true;
      ForceOwnerCrown.enabled = true;
      FriendsSince.enabled = true;
      ImageZoom = {
        enabled = true;
        invertScroll = true;
        nearestNeighbour = false;
        saveZoomValues = true;
        size = 340;
        square = false;
        zoom = 4;
        zoomSpeed = 0.5;
      };
      MessageLatency = {
        detectDiscordKotlin = true;
        enabled = true;
        latency = 1;
        showMillis = true;
      };
      NoPendingCount = {
        enabled = true;
        hideFriendRequestsCount = false;
        hideMessageRequestsCount = false;
        hidePremiumOffersCount = true;
      };
      PlatformIndicators = {
        badges = true;
        colorMobileIndicator = true;
        enabled = true;
        list = true;
        messages = true;
      };

      PronounDB = {
        enabled = true;
        pronounSource = 0;
        pronounsFormat = "LOWERCASE";
        showInMessages = true;
        showInProfile = true;
        showSelf = true;
      };

      RelationshipNotifier = {
        enabled = true;
        friendRequestCancels = true;
        friends = true;
        groups = true;
        notices = false;
        offlineRemovals = true;
        servers = true;
      };

      ReviewDB = {
        enabled = true;
        hideBlockedUsers = true;
        hideTimestamps = false;
        notifyReviews = true;
        reviewsDropdownState = true;
        showWarning = true;
      };

      SendTimestamps = {
        enabled = true;
        replaceMessageContents = true;
      };
      ServerInfo.enabled = true;

      ShikiCodeblocks = {
        bgOpacity = 100;
        enabled = true;
        theme = "https://raw.githubusercontent.com/shikijs/shiki/0b28ad8ccfbf2615f2d9d38ea8255416b8ac3043/packages/shiki/themes/dark-plus.json";
        tryHljs = "SECONDARY";
        useDevIcon = "GREYSCALE";
      };

      ShowHiddenChannels = {
        enabled = true;
        hideUnreads = true;
        showMode = 0;
      };
      TypingIndicator = {
        enabled = true;
        includeBlockedUsers = false;
        includeCurrentChannel = true;
        includeMutedChannels = false;
        indicatorMode = 3;
      };
      TypingTweaks = {
        alternativeFormatting = true;
        enabled = true;
        showAvatars = true;
        showRoleColors = true;
      };
      USRBG = {
        enabled = true;
        nitroFirst = true;
        voiceBackground = true;
      };
      UserVoiceShow = {
        enabled = true;
        showInUserProfileModal = true;
        showVoiceChannelSectionHeader = true;
      };
      VoiceDownload.enabled = true;
      VoiceMessages = {
        echoCancellation = true;
        enabled = true;
        noiseSuppression = true;
      };
      WebKeybinds.enabled = true;
      YoutubeAdblock.enabled = true;
      oneko.enabled = true;
      petpet.enabled = true;
    };

    finalSettings = mergeAttrsList [defaultSettings cfg.settings];
    finalPlugins = mergeAttrsList [defaultPlugins cfg.plugins];
    finalUserSettings = mergeAttrsList [defaultUserSettings cfg.userSettings {plugins = finalPlugins;}];
  in {
    meow.home.configFile."vesktop/quickCss.css" = {
      enable = true;
      text =
        if cfg.overrideQuickCss
        then cfg.quickCss
        else defaultQuickCss + cfg.quickCss;
    };

    nixpkgs.overlays = let
      makeSettings = pkgs.writeShellScript "create_discord_settings" ''
        ${createFile {
          name = "settings.json";
          attrs = finalSettings;
        }}
        ${createFile {
          name = "settings/settings.json";
          attrs = finalUserSettings;
        }}
      '';
    in [
      (final: prev: {
        vesktop = prev.vesktop.overrideAttrs (old: {
          postFixup =
            builtins.replaceStrings
            ["$out/bin/vesktop"] # makeWrapper command
            
            ["$out/bin/vesktop --run ${makeSettings}"]
            old.postFixup;
        });
      })
    ];
  });
}
