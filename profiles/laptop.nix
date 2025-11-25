{ system, config, pkgs, lib, nix-index-database, ... }: {
  imports = [
    ../lib/bluetooth.nix
    ../lib/cli.nix
    ../lib/cli-extra.nix
    ../lib/desktop.nix
    ../lib/desktop-extra.nix
    ../lib/desktop-theme.nix
    ../lib/devtools.nix
    ../lib/devtools-extra.nix
    ../lib/disks.nix
    ../lib/emacs.nix
    ../lib/firefox.nix
    ../lib/gnome.nix
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
    nix-index-database.homeModules.nix-index
  ];
  desktop = {
    enable = true;
  };
  home = {
    username = "sam";
    homeDirectory = "/home/sam";
    file = {
      ".ssh" = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/ssh";
      };
    };
    packages = with pkgs; [
      # zoom-us
      inkscape
      thunderbird
      zotero
      musescore
      wireguard-tools
      anki
      qFlipper
      cheese
      qbittorrent
      # vmware-horizon-client
      # libv4l
      # pcsclite
      # opensc
      # pcsctools
      libreoffice
      audacity
      josm
      qgis
      gimp
      geogebra
      # TODO: Fix Digikam
      # Broken in https://github.com/NixOS/nixpkgs/issues/449394
      #digikam
      # TODO: Get an alternative Discord client
      ffmpeg
      # publii
      keepassxc
      google-chrome
      tor-browser
      remmina
    ];
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
      includes = [
        {
          contents = {
            user = {
              name = "Samuel Grayson";
              email = "sam@samgrayson.me";
            };
          };
        }
      ];
      signing = {
        format = lib.mkForce "openpgp";
      };
    };
  };
  desktop = {
    bgimg = pkgs.fetchurl {
      url = "https://upload.wikimedia.org/wikipedia/commons/a/a8/Nighthawks_by_Edward_Hopper_1942.jpg";
      hash = "sha256-Rzp2AoAvjoB5t2elzSv+Eg6978tzviPyCbxLq2oIU6E=";
    };
  };
}
