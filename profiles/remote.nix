{ lib, nix-index-database, ... }: {
  imports = [
    ../lib/cli.nix
    ../lib/emacs.nix
    ../lib/devtools.nix
    ../lib/nixConf.nix
    ../lib/python.nix
    ../lib/home-manager.nix
    ../lib/starship.nix
    ../lib/xdg-ninja.nix
    ../lib/xonsh.nix
    ../lib/zsh.nix
    nix-index-database.homeModules.nix-index
  ];
  home = rec {
    username = "sagrays";
    homeDirectory = "/home/${username}";
  };
  targets = {
    genericLinux = {
      enable = true;
    };
  };
  nix = {
    settings = {
      ssl-cert-file = "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem";
    };
  };
  services = {
    lorri = {
      enable = lib.mkForce false;
    };
  };
}
