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
          settings = {
            "browser.startup.homepage" = "about:blank";
            "ui.systemUsesDarkTheme" = "1";
          };
          search = {
            default = "ddg";
            force = true;
          };
        };
      };
    };
  };
}
