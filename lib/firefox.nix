{ pkgs, ... }: {
  home = {
    sessionVariables = {
      # https://bbs.archlinux.org/viewtopic.php?id=267954
      MOZ_ENABLE_WAYLAND = 1;
      MOZ_DBUS_REMOTE = 1;
    };
    packages = with pkgs; [ google-chrome ];
  };
  programs = {
    firefox = {
      enable = true;
      profiles = {
        default = {
          id = 0;
          isDefault = true;
          bookmarks = { };
          settings = {
            "browser.startup.homepage" = "about:blank";
            "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
          };
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            bitwarden
            vimium
            darkreader
            consent-o-matic
            ublock-origin
            refined-github
            wikiwand-wikipedia-modernized
            auto-tab-discard
            foxytab
            wayback-machine
            return-youtube-dislikes
            languagetool
            # darkreader
            # tree-style-tab
          ];
          search = {
            default = "DuckDuckGo";
          };
        };
      };
    };
  };
}
