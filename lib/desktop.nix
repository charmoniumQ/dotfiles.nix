{ config, pkgs, lib, ... }: {
  config = {
    home = {
      packages = with pkgs; [
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
        libsForQt5.qt5.qtwayland
        hplipWithPlugin
        # Fixes Warning: qt.qpa.plugin: Could not find the Qt platform plugin "wayland" in ""
      ];
    };
    xdg = {
      mimeApps = {
        enable = true;
      };
    };
  };
  options = {
    desktop = {
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
