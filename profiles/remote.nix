{ nix-index-database, ... }: {
  imports = [
    ../lib/cli.nix
    ../lib/desktop.nix
    ../lib/nixConf.nix
    ../lib/python.nix
    ../lib/home-manager.nix
    ../lib/starship.nix
    ../lib/xdg-ninja.nix
    ../lib/xonsh.nix
    ../lib/zsh.nix
    nix-index-database.homeModules.nix-index
  ];
  home = {
    username = "grayson5";
    homeDirectory = "/home/grayson5";
  };
}
