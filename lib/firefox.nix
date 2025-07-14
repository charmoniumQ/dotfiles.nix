{ pkgs, lib, config, ... }: {
  home = {
    sessionVariables = lib.attrsets.optionalAttrs config.desktop.enable {
      # https://bbs.archlinux.org/viewtopic.php?id=267954
      MOZ_ENABLE_WAYLAND = 1;
      MOZ_DBUS_REMOTE = 1;
    };
  };
  programs = lib.attrsets.optionalAttrs config.desktop.enable {
    firefox = {
      enable = true;
      profiles = {
        default = {
          id = 0;
          isDefault = true;
          settings = {
            force = true;
            "browser.startup.homepage" = "about:blank";
            "ui.systemUsesDarkTheme" = "1";
          };
          search = {
            force = true;
            default = "ddg";
          };
          bookmarks = {
            force = true;
            settings = [
              {
                name = "Nixos options";
                url = "file:///home/sam/Documents/offline-pages/Appendix%20A.%20Configuration%20Options.html";
              }
              {
                name = "Home-manager options";
                url = "https://home-manager.dev/manual/unstable/options.xhtml";
              }
              {
                name = "NixOS manual";
                url = "file:///home/sam/Documents/offline-pages/NixOS%20Manual.html";
              }
              {
                name = "Nix fns";
                url = "https://teu5us.github.io/nix-lib.html";
              }
              {
                name = "Tuja Vortaro";
                url = "https://www.tujavortaro.net/?lingvo=en&vorto";
              }
              {
                name = "FRS/GRMS chart";
                url = "https://wiki.radioreference.com/index.php/FRS/GMRS_combined_channel_chart";
              }
            ];
          };
          extensions = {
            force = true;
            packages = with pkgs.nur.repos.rycee.firefox-addons; [
              vimium
              bitwarden
              consent-o-matic
              ublock-origin
              refined-github
              auto-tab-discard
              return-youtube-dislikes
              languagetool
              zotero-connector
              leechblock-ng
              # semantic-scholar
              gaoptout
              floccus
              clearurls
              old-reddit-redirect
              pushbullet
              # reddit-enhancement-suite # https://redditenhancementsuite.com/
              # reddit-comment-collapser # https://github.com/tom-james-watson/reddit-comment-collapser
              # https://addons.mozilla.org/en-US/firefox/addon/google-scholar-button/
              # web-archives
              # foxytab
              # wayback-machine
              # darkreader
              # tree-style-tab
            ];
          };
        };
      };
    };
  };
}
