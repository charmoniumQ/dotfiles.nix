{ pkgs, config, ... }:
let
  xonsh = pkgs.xonsh.override {
    extraPackages = pypkgs: with pypkgs; [
    # REPL
    ipython
    ptpython
    jedi
    jupyter

    # Utilities
    click
    typer
    tqdm
    virtualenv
    pyyaml
    dask

    # Scraping
    lxml
    beautifulsoup4
    requests

    # Science
    numpy
    scipy
    matplotlib
    pandas

    # Language server
    mypy
    python-lsp-server
    pylsp-mypy
    python-lsp-black
    python-lsp-ruff
  ] ++ [
      ply
      pygments
      prompt-toolkit
      setproctitle
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib-prompt-starship";
        version = "0.3.4";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "2106d4e13618891f12657bd44f32ffba48cdeca18239031369de1a6fa4ff152b";
        };
        checkPhase = ''
          runHook preCheck
          runHook postCheck
        '';
        makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.starship ]) ];
        propagatedBuildInputs = [ pkgs.starship ];
        doCheck = false;
      })
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib-jedi";
        version = "0.0.2";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "a0f75525a425214fa1039241b2ba18690967875056905f0a57cacca34959e48b";
        };
        propagatedBuildInputs = [ pypkgs.jedi ];
        # pythonImportsCheck = [ "xontrib.jedi" ];
        # nativeCheckInputs = [
        #   pypkgs.pytestCheckHook
        # ];
        # checkInputs = [ pypkgs.pytest ];
        doCheck = false;
      })
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib-fish-completer";
        version = "0.0.1";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "2abd62a25c7a0f1aa0c5536d5f0c1f82090badb1fd0658a51806196a1bd1fb76";
        };
        makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.fish ]) ];
        propagatedBuildInputs = [ pkgs.fish ];
        # pythonImportsCheck = [ "xontrib.fish_completer" ];
        # nativeCheckInputs = [ pypkgs.pytestCheckHook ];
        # checkInputs = [ pypkgs.pytest ];
        doCheck = false;
      })
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib_zoxide";
        version = "1.0.0";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "606be2220779e3dd550703c545953c1fd86125c91f6a53421f4597b0d09fb85c";
        };
        makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.zoxide ]) ];
        propagatedBuildInputs = [ pkgs.zoxide ];
        buildInputs = [ pypkgs.poetry-core ];
        # pythonImportsCheck = [ "xontrib.zoxide" ];
        # nativeCheckInputs = [ pypkgs.pytestCheckHook ];
        # checkInputs = [ pypkgs.pytest ];
        doCheck = false;
      })
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib-fzf-widgets";
        version = "0.0.4";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "12978eafd7371f015d17cb4ca5490536eedcae6c6a6cbd558d4839c13ccdcd49";
        };
        makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.fzf ]) ];
        propagatedBuildInputs = [ pkgs.fzf ];
        doCheck = false;
      })
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib-pipeliner";
        version = "0.3.4";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "7fcb548cf11061bc9cab56f76e15f03d4d9817d7e4a75511a830cb1f609e369a";
        };
        propagatedBuildInputs = [ pypkgs.six ];
        # pythonImportsCheck = [ "xontrib_pipeliner_asttokens" "xontrib.pipeliner" ];
        doCheck = false;
      })
      (pypkgs.buildPythonPackage rec {
        pname = "xontrib-readable-traceback";
        version = "0.4.0";
        src = pkgs.fetchPypi {
          inherit pname version;
          sha256 = "b5e841f0aa32dc941741cda784b67d5a4434352a4b9ffdd2da2cdb1dce850ebf";
        };
        doCheck = false;
        propagatedBuildInputs = [
          pypkgs.colorama
          (pypkgs.buildPythonPackage rec {
            pname = "backtrace";
            version = "0.2.1";
            src = pkgs.fetchPypi {
              inherit pname version;
              sha256 = "723ddc4c988c221a2d02455e51e8a07fe6245ded6b9e810c0ade429624b177f7";
            };
            propagatedBuildInputs = [ pypkgs.colorama ];
            # checkPhase = ''
            #   runHook preCheck
            #   runHook postCheck
            # '';
            # pythonImportsCheck = [ "backtrace" ];
            doCheck = false;
          })
        ];
      })
      # (pypkgs.buildPythonPackage rec {
      #   pname = "xontrib-abbrevs";
      #   version = "0.0.1";
      #   src = pkgs.fetchPypi {
      #     inherit pname version;
      #     sha256 = "6f7e7a1b5f83a63a83151fb85ac336a8cbbdd496873a7396e1aa77293b61ec16";
      #   };
      #   buildInputs = [
      #     pypkgs.setuptools
      #     pypkgs.poetry-core
      #     pypkgs.wheel
      #   ];
      #   propagatedBuildInputs = [ pypkgs.prompt-toolkit ];
      #   doCheck = false;
      #   # pythonImportsCheck = [ "xontrib.abbrevs" ];
      # })
      # TODO: consider https://dystroy.org/broot/
    ];
  };
in {
  home = {
    file = {
      xonsh = {
        source = ./xonsh/rc.xsh;
        target = ".config/xonsh/rc.xsh";
      };
      xonsh-aliases = {
        text = builtins.toJSON config.home.shellAliases;
        target = ".config/xonsh/aliases.json";
      };
    };
    packages = [
      xonsh
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
