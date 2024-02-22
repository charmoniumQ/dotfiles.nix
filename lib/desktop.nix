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
