{ config, pkgs, ... }:
{
  qt = {
    enable = true;
    style = {
      #package = pkgs.adwaita-qt;
      # Must be kvantum to use cattpuccin
      name = "kvantum";
    };
  };
  gtk = {
    enable = true;
    cursorTheme = {
      #package = pkgs.xorg.xcursorthemes;
      name = "arrow";
    };
    gtk3 = {
      bookmarks = [
        "file://${config.home.homeDirectory}/box"
        "file://${config.home.homeDirectory}/Downloads"
        "file://${config.home.homeDirectory}/Documents"
      ];
    };
  };
}
