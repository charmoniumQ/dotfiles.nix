{ pkgs, lib, config, ... }: {
  home = {
    sessionVariables = lib.attrsets.optionalAttrs config.desktop.enable {
      # https://bbs.archlinux.org/viewtopic.php?id=267954
      MOZ_ENABLE_WAYLAND = 1;
      MOZ_DBUS_REMOTE = 1;
    };
    packages = with pkgs; [
      google-chrome
      tor-browser
    ];
  };
  programs = lib.attrsets.optionalAttrs config.desktop.enable {
    firefox = {
      enable = true;
      profiles = {
        default = {
          id = 0;
          isDefault = true;
          bookmarks = { };
          settings = {
            "browser.startup.homepage" = "about:blank";
            # "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
            "ui.systemUsesDarkTheme" = "1";
          };
          extensions = {
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
            semantic-scholar
            gaoptout
            floccus
            clearurls
            user-agent-string-switcher
            canvasblocker
            # https://addons.mozilla.org/en-US/firefox/addon/google-scholar-button/
            # web-archives
            # foxytab
            # wayback-machine
            # darkreader
            # tree-style-tab
          ];
          };
          search = {
            default = "ddgGo";
            force = true;
          };
        };
      };
    };
  };
}
