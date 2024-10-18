{
  pkgs,
  config,
  ...
}: let
  python = pkgs.python312;
  pythonPkgs = pypkgs:
    with pypkgs;
      [
        # REPL
        ipython
        ptpython
        jedi
        jupyter
        ptpython
        ptpython

        # Misc
        hy
        panflute
        # TODO: fix macropy
        pygithub
        gitpython
        xxhash
        pendulum
        # dateutil.parser.parse still makes me really happy :)
        dateutil
        bitmath

        # Debugging
        icecream
        ipdb

        # CLI/UI
        cyclopts
        click
        typer
        tqdm
        rich
        textual

        # Data
        sqlalchemy
        sqlmodel
        pyyaml
        types-pyyaml
        pydantic
        rdflib

        # Testing
        pytest
        pytest-xdist

        # Scraping
        lxml
        beautifulsoup4
        types-beautifulsoup4
        requests
        types-requests
        aiohttp
        aiodns
        httpx

        # Emails
        imap-tools

        # Files
        aiofiles
        fsspec
        # TODO
        # sshfs

        # Science
        numpy
        scipy
        torch
        jax

        # Data science
        pandas
        polars
        pyarrow
        h5py
        xlrd
        networkx
        numpyro
        arviz

        # NLP
        # TODO: fix spacy
        levenshtein
        nltk

        # ML
        scikit-learn
        scikit-image

        # Distributed computation
        dask
        distributed
        (fabric.overrideAttrs (oldAttrs: rec {
           propagatedBuildInputs = (oldAttrs.propagatedBuildInputs or []) ++ [ pypkgs.pynacl ];
         }))

        # Servers
        flask
        fastapi
        django

        # Plotting
        matplotlib
        bokeh
        # TODO: holoviews
        seaborn
        plotext
        (pypkgs.buildPythonPackage rec {
          pname = "plotille";
          version = "5.0.0";
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "99e5ca51a2e4c922ead3a3b0863cc2c6a9a4b3f701944589df10f42ce02ab3dc"; # TODO
          };
        })

        # Language server
        mypy
        python-lsp-server
        pylsp-mypy
        # python-lsp-black
        python-lsp-ruff
      ]
      ++ (lib.lists.optional config.desktop.enable qtconsole)
      ++ [
        xonsh

        # For the fullest interactive user experience, these additional packages should also be installed:
        # according to https://xon.sh/packages.html
        pygments
        prompt-toolkit
        setproctitle

        # Fun xonsh plugins
        (pypkgs.buildPythonPackage rec {
          name = "xontrib-vox";
          version = "0.0.1";
          src = pkgs.fetchFromGitHub {
            owner = "xonsh";
            repo = "${name}";
            rev = "${version}";
            sha256 = "06csyhq0h63vq4w17q032dg1cx3j4xrr76maf7a0x4jqcvj4w79q";
          };
        })
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
          makeWrapperArgs = ["--prefix" "PATH" ":" (lib.makeBinPath [pkgs.starship])];
          propagatedBuildInputs = [pkgs.starship];
          doCheck = false;
        })
        (pypkgs.buildPythonPackage rec {
          pname = "xontrib_jedi";
          version = "0.1.1";
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "96a74d2dc68bd3d25130ca9e8918a048f10fb2c0b44b154c3c951f2b7f0f5041";
          };
          pyproject = true;
          propagatedBuildInputs = [pypkgs.jedi];
          buildInputs = [pypkgs.poetry-core pypkgs.xonsh];
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
          makeWrapperArgs = ["--prefix" "PATH" ":" (lib.makeBinPath [pkgs.fish])];
          propagatedBuildInputs = [pkgs.fish];
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
          makeWrapperArgs = ["--prefix" "PATH" ":" (lib.makeBinPath [pkgs.zoxide])];
          propagatedBuildInputs = [pkgs.zoxide];
          buildInputs = [pypkgs.poetry-core];
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
          makeWrapperArgs = ["--prefix" "PATH" ":" (lib.makeBinPath [pkgs.fzf])];
          propagatedBuildInputs = [pkgs.fzf];
          doCheck = false;
        })
        (pypkgs.buildPythonPackage rec {
          pname = "xontrib-pipeliner";
          version = "0.3.4";
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "7fcb548cf11061bc9cab56f76e15f03d4d9817d7e4a75511a830cb1f609e369a";
          };
          propagatedBuildInputs = [pypkgs.six];
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
              propagatedBuildInputs = [pypkgs.colorama];
              # checkPhase = ''
              #   runHook preCheck
              #   runHook postCheck
              # '';
              # pythonImportsCheck = [ "backtrace" ];
              doCheck = false;
            })
          ];
        })
        # TODO: consider https://dystroy.org/broot/
      ];
in {
  home = {
    packages = [
      (python.withPackages pythonPkgs)
      # TODO: fix pkgs.pipenv
      pkgs.pipx
      pkgs.hatch
      pkgs.virtualenv
    ];
  };
  programs = {
    poetry = {
      enable = true;
    };
    ruff = {
      enable = true;
      settings = {};
    };
  };
}
