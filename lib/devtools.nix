{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
      # System tools
      htop
      direnv
      hyperfine

      # Source code utils
      wgetpaste
      ripgrep
      fd
      meld

      # Containers
      bubblewrap
      # ctop

      # Pictures
      librsvg # needed to put svgs in pandoc
      # svgs are the best (or "based") format for including figures from PlantUML, Graphviz, etc. in documents
      pandoc
      plantuml
      graphviz

      # Project tools
      just

      # Rust tools
      # For long-term projects, you should use Crane in a Flake
      # For experimentation, it's nice to have a default version installed
      rustup

      # Data munging
      sqlite
      jq
      yq
      yj
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
      aliases = {
        "meld" = "!git difftool --tool meld --dir-diff";
        # https://stackoverflow.com/a/60501712/1078199
        # git for-each-ref refs/heads/ "--format=%(refname:short)" \
        # | while read branch; do
        #   mergeBase=$(git merge-base master $branch) \
        #   && [[ $(git cherry master $(git commit-tree $(git rev-parse $branch\^{tree}) -p $mergeBase -m _)) == "-"* ]] \
        #   && git branch -D $branch;
        # done
        "squash" = "!git reset --soft $(!git merge-base main $(!git rev-parse --abbrev-ref HEAD))";
      };
      extraConfig = {
        # "merge.conflictStyle" = "diff3";
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
