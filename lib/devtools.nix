{ pkgs, config, ... }:
let
  python = pkgs.python311;
  pythonPkgs = with python.pkgs; [
    click
    typer
    tqdm
    numpy
    scipy
    pandas
    matplotlib
    ipython
    requests
    lxml
    pyyaml
    mypy
    jupyter
    virtualenv

    # xonsh deps
    ply
    pygments
    prompt-toolkit
    setproctitle

    # Package managers:
    #pip # use virtualenv instead
    pipx

    (python.pkgs.toPythonModule pkgs.xonsh)
  ];
in
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
      # ((xonsh.override ({
      #   python3Packages = python.pkgs;
      # })).overrideAttrs (self: super: {
      #   propagatedBuildInputs = super.propagatedBuildInputs ++ pythonPkgs;
      # }))
      (python.withPackages (ps: pythonPkgs))
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
