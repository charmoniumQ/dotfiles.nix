{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      firefox
      keepassxc
      pcmanfm
      bitwarden
      imagemagick
      musescore
      thunderbird
      zotero
      betterbird
      xsel
    ];
  };
  dconf = {
    settings = {
      "org/gnome/desktop/background" = {
        draw-background = true;
        picture-options = "zoom";
        picture-uri = pkgs.stdenv.fetchurl {
          url = "https://upload.wikimedia.org/wikipedia/commons/a/a8/Nighthawks_by_Edward_Hopper_1942.jpg";
          hash = "";
        };
        show-desktop-icons = false;
      };
      "org/gnome/desktop/interface" = {
        clock-show-seconds = true;
        clock-show-weekday = true;
        show-battery-percentage = true;
      };
      "org/gnome/shell/extensions/dash-to-dock" = {
        dock-fixed = false;
      };
      "org/gnome/shell/extensions/desktop-icons" = {
        show-home = false;
        show-trash = false;
      };
      "org/gnome/settings-daemon/plugins/color" = {
        night-light-enabled = true;
        night-light-schedule-automatic = true;
        night-light-schedule-from = 0.0;
        night-light-schedule-to = 8.0;
        night-light-temperature = 2700;
      };
      "org/gnome/mutter" = {
        dynamic-workspaces = false;
        num-workspaces = 4;
      };
    };
  };
}
