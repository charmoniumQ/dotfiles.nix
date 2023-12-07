{ pkgs, ... }: {
  home = {
    sessionVariables = {
      MOZ_ENABLE_WAYLAND = 1;
    };
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
            i-dont-care-about-cookies
            # darkreader
            tree-style-tab
            # disconnect
            privacy-badger
            link-cleaner
          ];
          search = {
            default = "DuckDuckGo";
          };
        };
      };
    };
  };
}
