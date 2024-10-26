{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
      # System tools
      htop
      direnv
      trash-cli
      google-cloud-sdk
      asciinema
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
      # TODO: fix diffoscope

      # Containers
      bubblewrap
      #podman # Must be installed by host (NixOS or other)
      dive
      diffoci
      proot

      # Pictures
      xdot
      plantuml
      mermaid-cli
      ditaa

      librsvg # needed to put svgs in pandoc
      # svgs are the best (or "based") format for including figures from PlantUML, Graphviz, etc. in documents
      pandoc

      sqlitebrowser

      just

      ruby
      # I guess bundler is included with Ruby now?
      # bundler

      # Data munging
      graphviz
      xdot
      jq
      yq
      hdf5
      gephi

      # Java
      jdk
      maven
      gradle

      # VCS
      git
      tig
      gitg
      bfg-repo-cleaner
      mercurial
      subversion

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
