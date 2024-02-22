{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
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
    ];
    shellAliases = {
      ipy = "ipython";
      py = "python";
    };
    # For pipx and friends
    sessionPath = ["$HOME/.local/bin"];
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
