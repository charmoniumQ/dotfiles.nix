{ config, pkgs, lib, ... }:
{
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };
  programs = {
    home-manager = {
      enable = true;
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = {
        enable = true;
      };
    };
    emacs = {
      enable = true;
      extraPackages =
        if (import ./user.nix).installPackages
          then (import ./emacs/packages.nix)
          else (epkgs: [])
      ;
      #extraConfig = builtins.replaceStrings ["\n"] [" "] (builtins.readFile ./emacs/config.el);
    };
    zsh = import ./zsh/zsh.nix pkgs config;
    git = {
      enable = true;
      userEmail = (import ./user.nix).email;
      userName = (import ./user.nix).humanName;
      extraConfig = {
        init = {
          defaultBranch = true;
        };
      };
      lfs = {
        enable = true;
      };
    };
    starship = {
      enable = true;
      settings = {
        gcloud = {
          disabled = true;
        };
        package = {
          disabled = true;
        };
      };
    };
  };
  services = {
    emacs = {
      enable = true;
      client = {
        enable = true;
      };
      defaultEditor = true;
    };
  };
  home = {
    username = (import ./user.nix).unixName;
    homeDirectory = (import ./user.nix).homeDirectory;
    packages =
      if (import ./user.nix).installPackages
        then ((import ./nix/packages.nix) pkgs (import ./user.nix).gui)
        else []
    ;
    # For pipx and friends
    sessionPath = ["$HOME/.local/bin"];
    stateVersion = "22.11";
    sessionVariables = {
      #PS1 = "\$PREPEND_TO_PS1$PS1";
      # See xdg-ninja (https://github.com/b3nj5m1n/xdg-ninja)
      XDG_DATA_HOME = "${config.xdg.dataHome}";
      XDG_CONFIG_HOME = "${config.xdg.configHome}";
      XDG_STATE_HOME = "${config.xdg.stateHome}";
      XDG_CACHE_HOME = "${config.xdg.cacheHome}";
      HISTFILE = "${config.xdg.stateHome}/bash/history";
      LESSHISTFILE = "${config.xdg.stateHome}/less/history";
      CARGO_HOME = "${config.xdg.dataHome}/cargo";
      GNUPGHOME = "${config.xdg.dataHome}/gnupg";
      GOPATH = "${config.xdg.dataHome}/go";
      JULIA_DEPOT_PATH = "${config.xdg.dataHome}/julia:$JULIA_DEPOT_PATH";
      NODE_REPL_HISTORY = "${config.xdg.dataHome}/node_repl_history";
      OPAMROOT = "${config.xdg.dataHome}/opam";
      PSQL_HISTORY = "${config.xdg.dataHome}/psql_history";
      BUNDLE_USER_CACHE="${config.xdg.cacheHome}/bundle";
      BUNDLE_USER_PLUGIN="${config.xdg.dataHome}/bundle";
      LEIN_HOME = "${config.xdg.dataHome}/lein";
      JUPYTER_CONFIG_DIR = "${config.xdg.configHome}/jupyter";
      MPLAYER_HOME = "${config.xdg.configHome}/mplayer";
      IPYTHONDIR = "${config.xdg.configHome}/ipython";
      PARALLEL_HOME = "${config.xdg.configHome}/parallel";
      BUNDLE_USER_CONFIG="${config.xdg.configHome}/bundle";
      BOTO_CONFIG = "${config.xdg.configHome}/gsutil/config";
      _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${config.xdg.configHome}/java";
      CONDARC = "${config.xdg.configHome}/conda/condarc";
      KEEPASSDB = "$HOME/box/Database.kdbx";
      SPACK_USER_CONFIG_PATH="${config.xdg.dataHome}/spack-data";
      SPACK_USER_CACHE_PATH="${config.xdg.cacheHome}/spack";
    };
  };
  dconf =
    if (import ./user.nix).gui
      then import ./nix/dconf.nix
      else {}
    ;
}
