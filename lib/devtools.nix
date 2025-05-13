{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
      # System tools
      htop
      direnv
      trash-cli
      direnv
      hyperfine

      # Source code utils
      wgetpaste
      ripgrep
      fd
      delta
      difftastic
      bat
      meld

      # Containers
      bubblewrap
      ctop

      # Pictures
      xdot
      plantuml
      librsvg # needed to put svgs in pandoc
      # svgs are the best (or "based") format for including figures from PlantUML, Graphviz, etc. in documents
      pandoc

      # Project tools
      just

      # Rust tools
      # For long-term projects, you should use Crane in a Flake
      # For experimentation, it's nice to have a default version installed
      rustup

      # Data munging
      sqlite
      graphviz
      xdot
      jq
      yq
      hdf5

      # VCS
      git
      gitg
      bfg-repo-cleaner

      # Build systems
      cmake
      meson

      # GNU build-essentials
      gcc
      gnumake
      automake
      gdb

      # Clang equivalents
      #TODO: clang conflicts with gcc. How to rename the package?
      llvm
      lldb

      # Nix tools
      alejandra

      # Shell tools
      shellcheck
    ];
    shellAliases = {
      ipy = "ipython";
      py = "python";
    };
    # For pipx and friends
    sessionPath = ["$HOME/.local/bin"];
  };
  services = {
    lorri = {
      enable = true;
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
      aliases = {
        "meld" = "git difftool -t meld --dir-diff";
      };
      lfs = {
        enable = true;
      };
    };
  };
}
