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

      pandoc

      ruby

      # Data munging
      graphviz
      xdot
      jq
      yq
      hdf5

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
