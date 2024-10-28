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
        # We won't need this when #345326 drops
        # https://nixpk.gs/pr-tracker.html?pr=345326
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
        pkgs.nur.repos.xonsh-xontribs.xontrib-prompt-starship
        pkgs.nur.repos.xonsh-xontribs.xontrib-vox
        pkgs.nur.repos.xonsh-xontribs.xonsh-direnv
        pkgs.nur.repos.xonsh-xontribs.xontrib-fish-completer
        # pkgs.nur.repos.xonsh-xontribs.xontrib-jedi
        (pkgs.nur.repos.xonsh-xontribs.xontrib-zoxide.overrideAttrs (old: {
          postPatch = ''
            sed -ie "/xonsh.*=/d" pyproject.toml"
          '';
          src = pkgs.fetchFromGitHub {
            owner = "dyuri";
            repo = "xontrib-zoxide";
            rev = "36d3d0bc5945f2cd7aefdff598c6f7eeccfb1770";
            hash = "sha256-lYx5dfmVebSYls9rbvAeD8GdzYkwv/qy75xp1m+/mdA=";
          };
        }))
        # pkgs.nur.repos.xonsh-xontribs.xontrib-pipeliner
        pkgs.nur.repos.xonsh-xontribs.xontrib-sh
        # pkgs.nur.repos.xonsh-xontribs.xontrib-fzf-widgets

        # Use rich.traceback.install instead of readable_traceback
        # pkgs.nur.repos.xonsh-xontribs.xontrib-readable-traceback

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
