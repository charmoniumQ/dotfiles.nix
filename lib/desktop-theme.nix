{ config, pkgs, ... }:
{
  qt = {
    enable = true;
    platformTheme = {
      # package = pkgs.adwaita-qt;
      name = "adwaita-dark";
    };
    style = {
      #package = pkgs.adwaita-qt;
      name = "adwaita-dark";
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
    iconTheme = {
      #package = pkgs.adwaita-icon-theme;
      name = "adwaita-dark";
    };
    theme = {
      #package = pkgs.gnome-themes-extra;
      name = "adwaita-dark";
    };
  };
}
