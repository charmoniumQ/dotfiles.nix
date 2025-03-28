{ config, pkgs, lib, ... }: {
  config = {
    home = lib.attrsets.optionalAttrs config.desktop.enable {
      packages = with pkgs; [
        # TODO: lshw-gui conflicts with lshw
        pdftk
        libnotify
        dconf
        evince
        pcmanfm
        xarchiver
        lxterminal
        usbutils
        vlc
        gthumb
        distrobox
        system-config-printer
        yt-dlp
        # Fixes Warning: qt.qpa.plugin: Could not find the Qt platform plugin "wayland" in ""
        libsForQt5.qt5.qtwayland
        sqlitebrowser

        # echo alert-me-sound hello | at now+20m
        (pkgs.writeShellScriptBin "alert-me-sound" ''
          ${pkgs.ffmpeg}/bin/ffplay -v 0 -nodisp -autoexit ~/Documents/timer.mp3 &
          ${libnotify}/bin/notify-send --expire-time=10000 "Alert" "$1"
          wait
        '')
      ];
    };
    xdg = lib.attrsets.optionalAttrs config.desktop.enable {
      mimeApps = {
        enable = true;
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
