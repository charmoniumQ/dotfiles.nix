{ pkgs, lib, config, ... }: {
  home = {
    sessionVariables = lib.attrsets.optionalAttrs config.desktop.enable {
      # https://bbs.archlinux.org/viewtopic.php?id=267954
      MOZ_ENABLE_WAYLAND = 1;
      MOZ_DBUS_REMOTE = 1;
    };
  };
  # TODO: https://www.privacyguides.org/en/desktop-browsers/#firefox
  programs = lib.attrsets.optionalAttrs config.desktop.enable {
    firefox = {
      enable = true;
      configPath = "${config.xdg.configHome}/mozilla/firefox";
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
          bookmarks = let
            mkOfflineBookmark = {url, hash, ...}@opts: (builtins.removeAttrs opts ["url" "hash"]) // {
              url = "file://${pkgs.fetchurl { url = url; hash = hash; } }";
            };
          in {
            force = true;
            settings = [
              # (mkOfflineBookmark {
              #   name = "NixOS options";
              #   url = "https://nixos.org/manual/nixos/unstable/options";
              #   hash = "sha256-6ym4uTtU4XiF9a6h7x+72H7IjzB67MWltKtjTBUgHXA=";
              # })
              # (mkOfflineBookmark {
              #   name = "NixOS manual";
              #   url = "https://nixos.org/manual/nixos/unstable/";
              #   hash = "sha256-X2xac/PKEI1pOvd7y/jM4XSapLIzxFzmkzq5Q35AO68=";
              # })
              # (mkOfflineBookmark {
              #   name = "Home-manager options";
              #   url = "https://nix-community.github.io/home-manager/options.xhtml";
              #   hash = "sha256-OE05ZgBkpk0EfjSFi8XTWIe/EGfssc/SYiOYer8jpv8=";
              # })
              (mkOfflineBookmark {
                name = "Nix fns";
                url = "https://teu5us.github.io/nix-lib.html";
                hash = "sha256-PVKxgA0EWT/0tZaPHnr4BWnHCnr1zSyCCTK8tFEJuDU=";
              })
              (mkOfflineBookmark {
                name = "FRS/GRMS chart";
                url = "https://wiki.radioreference.com/index.php/FRS/GMRS_combined_channel_chart";
                hash = "sha256-sQXag/nHXJIdYyMbZhs+ffCvbL4SrcSMGZWNF8qeC0Y=";
              })
              {
                name = "Tuja Vortaro";
                url = "https://www.tujavortaro.net/?lingvo=en&vorto";
              }
            ];
          };
          extensions = {
            force = true;
            packages = with pkgs.nur.repos.rycee.firefox-addons; [
              auto-tab-discard
              bitwarden
              consent-o-matic
              leechblock-ng
              link-cleaner
              old-reddit-redirect
              pushbullet
              refined-github
              return-youtube-dislikes
              semantic-scholar
              sponsorblock
              tree-style-tab
              ublock-origin
              vimium
              web-archives
              zotero-connector
              # TODO: Package OpenSwitchMaps
              # TODO: self-host and enable languagetool
              # floccus
              # foxytab
              # gaoptout
              # https://addons.mozilla.org/en-US/firefox/addon/break-down-walls/
              # https://addons.mozilla.org/en-US/firefox/addon/google-scholar-1/
              # https://addons.mozilla.org/en-US/firefox/addon/google-scholar-button/
              # https://addons.mozilla.org/en-US/firefox/addon/google-scholar-search-engine/
              # https://addons.mozilla.org/en-US/firefox/addon/lazy-scholar/
              # https://addons.mozilla.org/en-US/firefox/addon/scholar-badge-for-zotero/
              # https://addons.mozilla.org/en-US/firefox/addon/sci-hub-in-google-scholar/
              # https://addons.mozilla.org/en-US/firefox/addon/sci-hub-scholar/
              # reddit-comment-collapser # https://github.com/tom-james-watson/reddit-comment-collapser
              # reddit-enhancement-suite # https://redditenhancementsuite.com/
              # wayback-machine
            ];
          };
        };
      };
    };
  };
}
