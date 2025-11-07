{ system, config, pkgs, lib, nix-index-database, ... }: {
  imports = [
    ../lib/cli.nix
    ../lib/devtools.nix
    ../lib/desktop.nix
    ../lib/emacs.nix
    ../lib/home-manager.nix
    ../lib/nixConf.nix
    ../lib/python.nix
    ../lib/starship.nix
    ../lib/texlive.nix
    ../lib/xdg-ninja.nix
    ../lib/xonsh.nix
    ../lib/zsh.nix
    nix-index-database.homeModules.nix-index
  ];
  home = rec {
    username = "sagrays";
    homeDirectory = "/home/${username}";
  };
  desktop = {
    enable = true;
    fontsize = 14;
  };
  programs = {
    git = {
      lfs = {
        enable = true;
      };
    };
  };
}
