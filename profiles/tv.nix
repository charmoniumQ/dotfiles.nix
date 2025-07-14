{ system, config, pkgs, lib, nix-index-database, ... }: {
  imports = [
    ../lib/cli.nix
    ../lib/cli-extra.nix
    ../lib/desktop.nix
    ../lib/desktop-extra.nix
    ../lib/disks.nix
    ../lib/firefox.nix
    ../lib/kodi.nix
    ../lib/home-manager.nix
    ../lib/nixConf.nix
    ../lib/starship.nix
    ../lib/xdg-ninja.nix
    ../lib/zsh.nix
    nix-index-database.homeModules.nix-index
  ];
  home = {
    username = "sysadmin";
    homeDirectory = "/home/sysadmin";
  };
  desktop = {
    enable = true;
    fontsize = 14;
  };
}
