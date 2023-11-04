{ pkgs, ... }:
let
  python = pkgs.python311;
  
  pythonPkgs = pypkgs: with pypkgs; [
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
    jedi

    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-prompt-starship";
      version = "0.3.4";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "2106d4e13618891f12657bd44f32ffba48cdeca18239031369de1a6fa4ff152b";
      };
      doCheck = false; # TODO: enable doCheck
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-jedi";
      version = "0.0.2";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "a0f75525a425214fa1039241b2ba18690967875056905f0a57cacca34959e48b";
      };
      doCheck = false; # TODO: enable check
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-fish-completer";
      version = "0.0.1";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "2abd62a25c7a0f1aa0c5536d5f0c1f82090badb1fd0658a51806196a1bd1fb76";
      };
      doCheck = false; # TODO: enable doCheck
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib_zoxide";
      version = "1.0.0";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "606be2220779e3dd550703c545953c1fd86125c91f6a53421f4597b0d09fb85c";
      };
      buildInputs = [ python.pkgs.poetry-core ];
      testInputs = [ python.pkgs.pytest ];
      doCheck = false; # TODO: enable check
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-fzf-widgets";
      version = "0.0.4";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "12978eafd7371f015d17cb4ca5490536eedcae6c6a6cbd558d4839c13ccdcd49";
      };
      doCheck = false; # TODO: enable check
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-pipeliner";
      version = "0.3.4";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "7fcb548cf11061bc9cab56f76e15f03d4d9817d7e4a75511a830cb1f609e369a";
      };
      propagatedBuildInputs = [ python.pkgs.six ];
      doCheck = false; # TODO: enable check
    })
    # TODO: consider https://dystroy.org/broot/
  ];
in {
  home = {
    file = {
      xonsh = {
        text = ''
          xontrib load prompt_starship
          xontrib load jedi
          xontrib load zoxide
          xontrib load fzf-widgets
          xontrib load pipeliner
        '';
        target = ".config/xonsh/rc.xsh";
      };
    };
    packages = [
      # (python.withPackages (pypkgs: (pythonPkgs pypkgs) ++ (python.pkgs.toPythonModule pkgs.xonsh)))
      (pkgs.xonsh.override {
        extraPackages = pypkgs: (pythonPkgs pypkgs);
      })
      pkgs.fish # for xontrib-fish-completer
      pkgs.starship
    ];
  };
  programs = {
    fzf = {
      enable = true;
    };
    zoxide = {
      enable = true;
      options = [ "--hook" "pwd" ];
      # TODO: zoxide.el
    };
  };
}
