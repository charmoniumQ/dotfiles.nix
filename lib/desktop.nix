{ config, pkgs, lib, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        libnotify
        dconf
        evince
        pcmanfm
        peazip
        lxterminal
        usbutils
        vlc
      ];
    };
    xdg = {
      mimeApps = {
        enable = true;
        defaultApplications = let
          archiver = "${pkgs.peazip}/share/applications/peazip-open.desktop";
          browser = "${config.programs.firefox.package}/share/applications/firefox.desktop";
        in {
          "inode/directory" = "${pkgs.pcmanfm}/share/applications/pcmanfm.desktop";
          "application/pdf" = "${pkgs.evince}/share/applications/org.gnome.Evince.desktop";
          "application/x-zip-compressed" = archiver;
          "application/gzip" = archiver;
          "application/x-gzip" = archiver;
          "application/tar" = archiver;
          "application/x-tar" = archiver;
          "application/tar+gzip" = archiver;
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-www-browser" = browser;
          "text/html" = browser;
        };
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
