{ config, pkgs, lib, ... }: {
  config = {
    home = lib.attrsets.optionalAttrs config.desktop.enable {
      packages = with pkgs; [
        # TODO: lshw-gui conflicts with lshw
        pdftk
        gthumb
        distrobox
        yt-dlp
        # Fixes Warning: qt.qpa.plugin: Could not find the Qt platform plugin "wayland" in ""
        libsForQt5.qt5.qtwayland

      ];
    };
  };
}
