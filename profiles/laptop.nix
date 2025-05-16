{ system, config, pkgs, lib, nix-index-database, ... }: {
  imports = [
    ../lib/cli.nix
    ../lib/cli-extra.nix
    ../lib/desktop.nix
    ../lib/desktop-extra.nix
    ../lib/devtools.nix
    ../lib/devtools-extra.nix
    ../lib/disks.nix
    ../lib/emacs.nix
    ../lib/firefox.nix
    ../lib/libreoffice.nix
    ../lib/gnome.nix
    ../lib/home-manager.nix
    ../lib/hyprland.nix
    ../lib/keyring.nix
    ../lib/nextcloud.nix
    ../lib/nixConf.nix
    ../lib/printer-scanner.nix
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
    file = {
      ".ssh" = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/ssh";
      };
    };
    packages = with pkgs; [
      zoom-us
      inkscape
      thunderbird
      signal-desktop
      zotero
      element-desktop
      musescore
      wireguard-tools
      anki
      pkgs.cheese
      # vmware-horizon-client
      # libv4l
      # pcsclite
      # opensc
      # pcsctools
      audacity
      gimp
      slack
      # TODO: Get an alternative Discord client
      ffmpeg
      publii
      keepassxc
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
      delta = {
        enable = true;
      };
      extraConfig = {
        github = {
          user = "charmoniumQ";
        };
      };
      lfs = {
        enable = true;
      };
    };
    firefox = {
      profiles = {
        default = {
          bookmarks = {
            force = true;
            settings = [
              {
                name = "Nixos options";
                url = "file:///home/sam/Documents/offline-pages/Appendix%20A.%20Configuration%20Options.html";
              }
              {
                name = "Home-manager options";
                url = "https://home-manager.dev/manual/unstable/options.xhtml";
              }
              {
                name = "NixOS manual";
                url = "file:///home/sam/Documents/offline-pages/NixOS%20Manual.html";
              }
              {
                name = "Nix fns";
                url = "https://teu5us.github.io/nix-lib.html";
              }
              {
                name = "Tuja Vortaro";
                url = "https://www.tujavortaro.net/?lingvo=en&vorto";
              }
              {
                name = "FRS/GRMS chart";
                url = "https://wiki.radioreference.com/index.php/FRS/GMRS_combined_channel_chart";
              }
            ];
          };
          extensions = {
            packages = with pkgs.nur.repos.rycee.firefox-addons; [
              vimium
              bitwarden
              consent-o-matic
              ublock-origin
              refined-github
              auto-tab-discard
              return-youtube-dislikes
              languagetool
              zotero-connector
              leechblock-ng
              semantic-scholar
              gaoptout
              floccus
              clearurls
              user-agent-string-switcher
              canvasblocker
              old-reddit-redirect
              # reddit-enhancement-suite # https://redditenhancementsuite.com/
              # reddit-comment-collapser # https://github.com/tom-james-watson/reddit-comment-collapser
              # https://addons.mozilla.org/en-US/firefox/addon/google-scholar-button/
              # web-archives
              # foxytab
              # wayback-machine
              # darkreader
              # tree-style-tab
            ];
            force = true;
          };
        };
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
