# This is a failed attempt

{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sam";
  home.homeDirectory = "/home/sam";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  programs.direnv = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    shellAliases = {
      l = "exa -alrs modified";
      emacs = "emacs --user ''";
    };
    initExtra = ''[ -n "$PROFILE" ] || . ~/.profile'';

    enableAutosuggestions = true;
    plugins = [
      # TODO: fzf, git
    ];
    oh-my-zsh = {
      plugins = [];
      enable = true;
      theme = "robbyrussel";
    };
  };

  programs.git = {
    enable = true;
    userEmail = "sam@samgrayson.me";
    userName = "Samuel Grayson";
  };

  programs.htop = {
    enable = true;
  };

  programs.fzf = {
    enable = true;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.monokai-theme
      epkgs.helm
      epkgs.el-get
      # TODO: this
    ];
  };

  programs.tmux = {
    enable = true;
    shortcut = "h";
    keyMode = "emacs";
    clock24 = true;
    plugins = [
      # TODO
    ];
  };

  programs.ssh = {
    enable = true;    
  };

  programs.kitty = {
    enable = true;
    font = {
      name = "Fira Code";
      package = pkgs."fira-code";
    };
    settings = {
      background_opacity = "0.70";
      term = "xterm-256color";
    };
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
    };
  };

  home.packages = with pkgs; [
    exa
    nox
    fd
    ripgrep
  ];

}
