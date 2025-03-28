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
      ctop

      # Pictures
      xdot
      plantuml
      mermaid-cli
      d2
      ditaa
      librsvg # needed to put svgs in pandoc
      # svgs are the best (or "based") format for including figures from PlantUML, Graphviz, etc. in documents
      pandoc

      # Project tools
      just

      # JS tools
      nodejs
      yarn-berry

      # Ruby tools
      ruby
      rubocop
      # I guess bundler is included with Ruby now?
      # bundler

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
      gephi
      sqlitebrowser
      miller
      pup

      # Java tools
      jdk
      maven
      gradle

      # VCS
      git
      git-machete
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

      # VM tools
      vagrant

      # Nix tools
      alejandra

      # Sem Web tools
      protege-distribution
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
        commit = {
          gpgsign = true;
        };
        gpg = {
          format = "ssh";
        };
        user = {
          signingkey = "~/.ssh/id_ed25519.pub";
        };
      };
      lfs = {
        enable = true;
      };
    };
  };
}
