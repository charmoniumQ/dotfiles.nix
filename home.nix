{ config, pkgs, lib, ... }:

{
  programs = {
    home-manager = {
      # Let Home Manager install and manage itself.
      enable = true;
    };
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };
    zsh = {
      enable = true;
      enableCompletion = true;
      autocd = true;
      dotDir = ".config/zsh";
      envExtra = lib.concatStringsSep "\n" [
        "source \${HOME}/.nix-profile/etc/profile.d/nix.sh"
        "export KEEPASSDB=\${HOME}/box/Database.kdbx"
        "export PATH=\"\${HOME}/.local/bin:\${PATH}\""
        "export PROMPT=\"$(if [[ -n \${POETRY_ACTIVE} ]]; then echo 'poetry '; else fi)\${PROMPT}\""
      ];
      sessionVariables = {
        DISABLE_UNTRACKED_FILES_DIRTY = "true";
        COMPLETION_WAITING_DOTS = "true";
      };
      shellAliases = {
        l = "exa -alrs modified";
        ipy = "ipython";
        py = "python";
        pass = "pwgen --capitalize --numerals --symbols --ambiguous 20 1";
        passphrase = "xkcdpass --wordfile eff-long --numwords 14";
      };
      plugins = [
        {
          name = "zsh-syntax-highlighting";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-syntax-highlighting";
            rev = "0.7.0";
            sha256 = "0s1z3whzwli5452h2yzjzzj27pf1hd45g223yv0v6hgrip9f853r";
          };
        }
        {
          name = "zsh-autosuggestions";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-autosuggestions";
            rev = "v0.6.4";
            sha256 = "0h52p2waggzfshvy1wvhj4hf06fmzd44bv6j18k3l9rcx6aixzn6";
          };
        }
        {
          name = "spaceship-prompt";
          src = pkgs.fetchFromGitHub {
            owner = "denysdovhan";
            repo = "spaceship-prompt";
            rev = "v3.11.2";
            sha256 = "1q7m9mmg82n4fddfz01y95d5n34xnzhrnn1lli0vih39sgmzim9b";
          };
        }
      ];
      oh-my-zsh = {
        enable = true;
        plugins = [
          "fzf"
          "git"
        ];
        theme = "robbyrussell";
      };
    };
    git = {
      enable = true;
      userEmail = "sam@samgrayson.me";
      userName = "Samuel Grayson";
    };
    tmux = {
      enable = true;
      prefix = "C-h";
      clock24 = true;
      keyMode = "emacs";
      extraConfig = lib.concatStringsSep "\n" [
        "set -g mouse on"
        "unbind R"
        "bind R source-file ~/.config/tmux/tmux.conf"
      ];
      plugins = [
        pkgs.tmuxPlugins.extrakto
        {
          plugin = pkgs.tmuxPlugins.dracula;
          extraConfig = lib.concatStringsSep "\n" [
            "set -g @dracula-show-battery true"
            "set -g @dracula-show-network true"
            "set -g @dracula-show-weather false"
            "set -g @dracula-show-fahrenheit true"
            "set -g @dracula-show-powerline true"
            "set -g @dracula-military-time true"
            "set -g @dracula-show-left-icon session"
            "set -g @dracula-border-contrast true"
            "set -g @dracula-cpu-percent true"
          ];
        }
      ];
    };
    kitty = {
      enable = true;
      font = {
        name = "Fira Code";
        # package = pkgs.fira-code;
      };
      keybindings = {
        "ctrl+shift+minus" = "no_op";
        "ctrl+shift+plus" = "no_op";
      };
      settings = {
        clipboard_control = "no-append";
        hide_window_decorations = "yes";
        background_opacity = "0.70";
        term = "xterm-256color";
      };
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "sam";
    homeDirectory = "/home/sam";
    
    # TODO: Separate user/username from the rest
    packages = [

      # CLI
      pkgs.tmux
      pkgs.emacs
      pkgs.git
      pkgs.gnumake # needed for Emacs
      pkgs.zsh
      pkgs.exa
      pkgs.fzf
      pkgs.bpytop
      pkgs.htop
      pkgs.glances
      pkgs.lnav
      pkgs.dtrx
      pkgs.ncdu
      pkgs.trash-cli
      pkgs.docker
      pkgs.google-cloud-sdk
      pkgs.rclone
      pkgs.mosh
      pkgs.moreutils
      # Keepassxc has a CLI and a GUI
      pkgs.keepassxc
      pkgs.jq
      pkgs.yq
      pkgs.fd
      pkgs.tig
      pkgs.icdiff
      pkgs.bfg-repo-cleaner
      pkgs.ripgrep
      pkgs.xsel
      pkgs.python39
      pkgs.ruby_2_7
      pkgs.gnupg
      pkgs.bat
      # pkgs.ipython
      #pkgs.nodePackages.npm
      # IPython, numpy, pandas, tqdm, scipy, matplotlib, PyQt5, requests, PyYAML, jupyter-notebook
      pkgs.magic-wormhole

      # GUI
      pkgs.meld
      pkgs.gitg

      # requires OpenGL, which Nix is bad at supporting.
      # I install this through the system package manager instead.
      #pkgs.kitty
      #pkgs.firefox
      #pkgs.google-chrome
      # snap install zoom spotify discord slack fractal telegram beeper
      pkgs.mtr
      pkgs.pwgen
      pkgs.xkcdpass

      pkgs.mpv
      pkgs.mplayer
    ];

    file = {
      ".config/emacs/init.el" = {
        source = ./.config/emacs/init.el;
      };
      ".local/bin/quote" = {
        source = ./.local/bin/quote;
      };
      ".local/bin/r" = {
        source = ./.local/bin/r;
      };
      ".local/bin/emc" = {
        source = ./.local/bin/emc;
      };
      ".local/bin/keepass2ssh" = {
        source = ./.local/bin/keepass2ssh;
      };
    };
    
    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "21.05";
  };
  dconf = {
    settings = {
      "org/gnome/desktop/background" = {
        draw-background = true;
        picture-options = "zoom";
        picture-uri = "https://upload.wikimedia.org/wikipedia/commons/a/a8/Nighthawks_by_Edward_Hopper_1942.jpg";
        show-desktop-icons = false;
      };
      "org/gnome/desktop/interface" = {
        clock-show-seconds = true;
        clock-show-weekday = true;
        show-battery-percentage = true;
      };
      "org/gnome/shell/extensions/dash-to-dock" = {
        dock-fixed = false;
      };
      "org/gnome/shell/extensions/desktop-icons" = {
        show-home = false;
        show-trash = false;
      };
      "org/gnome/shell" = {
        favorite-apps = [
          "firefox.desktop"
          "emacs.desktop"
          "kitty.desktop"
          "slack_slack.desktop"
        ];
      };
      "org/gnome/settings-daemon/plugins/color" = {
        night-light-enabled = true;
        night-light-schedule-automatic = true;
        night-light-schedule-from = 0.0;
        night-light-schedule-to = 8.0;
        night-light-temperature = 2700;
      };
      "org/gnome/mutter" = {
        dynamic-workspaces = false;
        num-workspaces = 4;
      };
    };
  };
}
