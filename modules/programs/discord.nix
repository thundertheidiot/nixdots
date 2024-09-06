{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (lib) mkIf;
  inherit (mlib) homeModule;
  inherit (builtins) toJSON;
in {
  config = mkIf (config.meow.program.discord && config.meow.program.discordConfig) {
    meow.home.configFile."vesktop/settings.json" = {
      enable = true;
      text = toJSON {
        discordBranch = "stable";
        splashColor = "rgb(205, 214, 244)";
        splashBackground = "rgb(30, 30, 46)";
        staticTitle = true;
        minimizeToTray = false;
        tray = false;
      };
    };

    meow.home.configFile."vesktop/settings/quickCss.css" = {
      enable = true;
      text = ''
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
    };

    meow.home.configFile."vesktop/settings/settings.json" = {
      enable = true;
      text = toJSON {
        autoUpdate = true;
        autoUpdateNotification = true;
        useQuickCss = true;
        themeLinks = [
          "https://catppuccin.github.io/discord/dist/catppuccin-mocha.theme.css"
        ];
        enabledThemes = [];
        enableReactDevtools = false;
        frameless = false;
        transparent = false;
        winCtrlQ = false;
        disableMinSize = true;
        winNativeTitleBar = false;
        plugins = {
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
          BetterSessions = {
            backgroundCheck = false;
            checkInterval = 20;
            enabled = false;
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
          ChatInputButtonAPI.enabled = true;
          CommandsAPI.enabled = true;
          CustomIdle = {
            enabled = false;
            idleTimeout = 10;
            remainInIdle = true;
          };
          Decor = {
            agreedToGuidelines = false;
            enabled = true;
          };
          Experiments = {
            enabled = false;
            toolbarDevMenu = false;
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
          FixSpotifyEmbeds = {
            enabled = false;
            volume = 10;
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
          ImplicitRelationships = {
            enabled = false;
            sortByAffinity = true;
          };
          InvisibleChat = {
            enabled = false;
            savedPasswords = "password, Password";
          };
          LoadingQuotes = {
            additionalQuotes = "";
            additionalQuotesDelimiter = "|";
            enableDiscordPresetQuotes = false;
            enablePluginPresetQuotes = true;
            enabled = false;
            replaceEvents = true;
          };
          MemberListDecoratorsAPI.enabled = true;
          MessageAccessoriesAPI.enabled = true;
          MessageDecorationsAPI.enabled = true;
          MessageEventsAPI.enabled = true;
          MessageLatency = {
            detectDiscordKotlin = true;
            enabled = true;
            latency = 1;
            showMillis = true;
          };
          MessageLogger = {
            deleteStyle = "text";
            enabled = false;
            ignoreBots = false;
            ignoreChannels = "";
            ignoreGuilds = "";
            ignoreSelf = false;
            ignoreUsers = "";
            logDeletes = true;
            logEdits = true;
          };
          MessageUpdaterAPI.enabled = true;
          NoMosaic = {
            enabled = false;
            inlineVideo = true;
          };
          NoPendingCount = {
            enabled = true;
            hideFriendRequestsCount = false;
            hideMessageRequestsCount = false;
            hidePremiumOffersCount = true;
          };
          NoTrack = {
            disableAnalytics = true;
            enabled = true;
          };

          OpenInApp = {
            enabled = false;
            epic = true;
            spotify = true;
            steam = true;
            tidal = true;
          };

          PartyMode = {
            enabled = false;
            superIntensePartyMode = 0;
          };

          PictureInPicture = {
            enabled = false;
            loop = true;
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

          Settings = {
            enabled = false;
            settingsLocation = "aboveNitro";
          };
          ShikiCodeblocks = {
            bgOpacity = 100;
            enabled = true;
            theme = "https://raw.githubusercontent.com/shikijs/shiki/0b28ad8ccfbf2615f2d9d38ea8255416b8ac3043/packages/shiki/themes/dark-plus.json";
            tryHljs = "SECONDARY";
            useDevIcon = "GREYSCALE";
          };

          ShowConnections = {
            enabled = false;
            iconSize = 32;
            iconSpacing = 1;
          };
          ShowHiddenChannels = {
            enabled = true;
            hideUnreads = true;
            showMode = 0;
          };
          ShowHiddenThings = {
            disableDisallowedDiscoveryFilters = true;
            disableDiscoveryFilters = true;
            enabled = false;
            showInvitesPaused = true;
            showModView = true;
            showTimeouts = true;
          };

          ShowTimeoutDuration = {
            displayStyle = "ssalggnikool";
            enabled = false;
          };

          SpotifyCrack = {
            enabled = false;
            keepSpotifyActivityOnIdle = false;
            noSpotifyAutoPause = true;
          };

          Summaries = {
            enabled = false;
            summaryExpiryThresholdDays = 3;
          };

          Translate = {
            autoTranslate = false;
            enabled = false;
            showChatBarButton = true;
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
          UserSettingsAPI.enabled = true;
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
          WebContextMenus = {
            addBack = true;
            enabled = false;
          };
          WebKeybinds.enabled = true;
          YoutubeAdblock.enabled = true;
          oneko.enabled = true;
          petpet.enabled = true;
        };
      };
    };
  };
}
