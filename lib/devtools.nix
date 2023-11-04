{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
      comma
      htop
      direnv
      trash-cli
      google-cloud-sdk
      asciinema
      git
      pdftk
      tig
      wgetpaste
      ripgrep
      jq
      yq
      fd
      icdiff
      bfg-repo-cleaner
      pandoc
      ruby
      bat
      graphviz
      pipenv
      gitg
      meld
      xdot
      starship
    ];
    shellAliases = {
      ipy = "ipython";
      py = "python";
    };
    # For pipx and friends
    sessionPath = ["$HOME/.local/bin"];
    sessionVariables = {
      XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
    };
  };
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = config.programs.zsh.enable;
      nix-direnv = {
        enable = true;
      };
    };
    lsd = {
      enable = true;
    };
    man = {
      enable = true;
    };
    git = {
      enable = true;
      extraConfig = {
        init = {
          defaultBranch = true;
        };
      };
      lfs = {
        enable = true;
      };
    };
  };
}
