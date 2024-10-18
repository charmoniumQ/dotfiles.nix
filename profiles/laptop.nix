{ system, config, pkgs, lib, nix-index-database, ... }: {
  imports = [
    ../lib/cli.nix
    ../lib/desktop.nix
    ../lib/devtools.nix
    ../lib/disks.nix
    ../lib/emacs.nix
    ../lib/firefox.nix
    ../lib/libreoffice.nix
    ../lib/home-manager.nix
    ../lib/hyprland.nix
    ../lib/keyring.nix
    ../lib/nextcloud.nix
    ../lib/nixConf.nix
    ../lib/python.nix
    ../lib/starship.nix
    ../lib/texlive.nix
    ../lib/xdg-ninja.nix
    ../lib/xonsh.nix
    ../lib/zsh.nix
    nix-index-database.hmModules.nix-index
  ];
  desktop = {
    enable = true;
  };
  home = {
    username = "sam";
	  homeDirectory = "/home/sam";
    packages = with pkgs; [
      zoom-us
      inkscape
      thunderbird
      signal-desktop
      zotero
      element-desktop
      musescore
      wireguard-tools
      # anki
      pkgs.cheese
      # vmware-horizon-client
      # libv4l
      # pcsclite
      # opensc
      # pcsctools
      audacity
      gimp
      slack
      ffmpeg
    ] ++ [  ];
    file = {
      Zotero = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/Zotero";
      };
    };
  };
  nixpkgs = {
    config = {
      permittedInsecurePackages = [
        "zotero-6.0.27"
      ];
    };
  };
  programs = {
    git = {
      userName = "Samuel Grayson";
      userEmail = "sam@samgrayson.me";
    };
  };
  desktop = {
    bgimg = pkgs.fetchurl {
      url = "https://upload.wikimedia.org/wikipedia/commons/a/a8/Nighthawks_by_Edward_Hopper_1942.jpg";
      hash = "sha256-Rzp2AoAvjoB5t2elzSv+Eg6978tzviPyCbxLq2oIU6E=";
    };
  };
}
