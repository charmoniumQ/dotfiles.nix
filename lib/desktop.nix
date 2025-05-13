{ config, pkgs, lib, ... }: {
  config = {
    home = lib.attrsets.optionalAttrs config.desktop.enable {
      packages = with pkgs; [
        libnotify
        dconf
        evince
        pcmanfm
        xarchiver
        lxterminal
        vlc
        usbutils
        system-config-printer
        # Fixes Warning: qt.qpa.plugin: Could not find the Qt platform plugin "wayland" in ""
        libsForQt5.qt5.qtwayland

        # echo alert-me-sound hello | at now+20m
        (pkgs.writeShellScriptBin "alert-me-sound" ''
          ${pkgs.ffmpeg}/bin/ffplay -v 0 -nodisp -autoexit ~/Documents/timer.mp3 &
          ${libnotify}/bin/notify-send --expire-time=10000 "Alert" "$1"
          wait
        '')
      ];
    };
    xdg = lib.attrsets.optionalAttrs config.desktop.enable {
      # env XDG_UTILS_DEBUG_LEVEL=3 xdg-mime query default image/png
      mimeApps = {
        enable = true;
        associations.added = {
          "application/pdf" = ["org.gnome.Evince.desktop"];
        };
        defaultApplications = {
          "application/pdf" = ["org.gnome.Evince.desktop"];
        };
      };
    };
  };
  options = {
    desktop = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      guiFramework = lib.mkOption {
        type = lib.types.enum [ "qt" "gtk" ];
        default = "gtk";
      };
      style = lib.mkOption {
        type = lib.types.enum [ "nord" "gruvbox" ];
        default = "nord";
      };
      bgimg = lib.mkOption {
        type = lib.types.path;
      };
    };
  };
}
