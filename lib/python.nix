{
  pkgs,
  config,
  ...
}: let
  python = pkgs.python313;
  disablePytest = pypkg: builtins.trace
    "Building ${pypkg.name} without tests against my better judgement"
    pypkg.overridePythonAttrs {
      pytestCheckPhase = "true";
    }
  ;
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
        # macropy is no longer maintained
        pygithub
        gitpython
        xxhash
        pendulum
        # dateutil.parser.parse still makes me really happy :)
        dateutil
        bitmath
        psutil
        marko

        # Debugging
        icecream
        ipdb

        # CLI/UI
        cyclopts
        click
        typer
        tqdm
        types-tqdm
        rich
        textual

        # Data
        sqlalchemy
        sqlmodel
        pyyaml
        types-pyyaml
        pydantic
        rdflib

        # Machine learning
        huggingface-hub
        keras

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
        # (pkgs.python3.pkgs.buildPythonPackage {
        #   pname = "domonic";
        #   pyproject = true;
        #   version = "0.9.13";
        #   src = pkgs.fetchFromGitHub {
        #     owner = "byteface";
        #     repo = "domonic";
        #     tag = "0.9.13";
        #     hash = "sha256-Z5ewo4WTuAf7+jHjqP5qdCdXrevWI7NY3KFLWhN6fEQ=";
        #   };
        #   pythonRuntimeDepsCheckHook = "";
        #   dependencies = [
        #     pypkgs.elementpath
        #     pypkgs.python-dateutil
        #     pypkgs.requests
        #     pypkgs.urllib3
        #     pypkgs.html5lib
        #     pypkgs.cssselect
        #   ];
        #   nativeBuildInputs = [
        #     pypkgs.setuptools
        #   ];
        # })

        # Emails
        imap-tools

        # Files
        aiofiles
        fsspec
        sshfs

        # Data science
        numpy
        nptyping
        beartype
        scipy
        torch
        jax
        torch
        pandas
        polars
        pyarrow
        h5py
        xlrd

        # ML
        scikit-learn
        umap-learn

        # Networks
        networkx
        rustworkx

        # Data analysis
        # (numpyro.override {
        #   dm-haiku = pypkgs.dm-haiku.overridePythonAttrs {
        #     disabled = false;
        #   };
        # }) # TODO
        #arviz # TODO

        # GIS
        geopandas
        pyproj
        gpxpy
        # opensfm

        # Astro
        astropy

        # NLP
        spacy # TODO
        spacy-models.en_core_web_lg
        levenshtein
        nltk

        # Image processing
        pillow
        types-pillow
        opencv4
        scikit-image

        # Distributed computation
        dask
        distributed
        fabric

        # Web servers
        flask
        fastapi
        django

        # Plotting
        matplotlib
        bokeh
        holoviews
        altair
        vega
        seaborn
        plotext
        plotille

        # Language server
        mypy
        python-lsp-server
        pylsp-mypy
        python-lsp-black
        python-lsp-ruff
        isort
        nose2
      ]
      ++ [
        # Xonsh and plugins
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
        # TODO: Straighten out the plugin nonsense
        #pkgs.nur.repos.xonsh-xontribs.xontrib-jedi
        (pkgs.nur.repos.xonsh-xontribs.xontrib-zoxide.overrideAttrs (old: {
          postPatch = ''
            sed -ie "/xonsh.*=/d" pyproject.toml
          '';
          src = pkgs.fetchFromGitHub {
            owner = "dyuri";
            repo = "xontrib-zoxide";
            rev = "36d3d0bc5945f2cd7aefdff598c6f7eeccfb1770";
            hash = "sha256-lYx5dfmVebSYls9rbvAeD8GdzYkwv/qy75xp1m+/mdA=";
          };
        }))
        #pkgs.nur.repos.xonsh-xontribs.xontrib-pipeliner
        pkgs.nur.repos.xonsh-xontribs.xontrib-sh
        #pkgs.nur.repos.xonsh-xontribs.xontrib-fzf-widgets

        # TODO: Use rich.traceback.install instead of readable_traceback
        #pkgs.nur.repos.xonsh-xontribs.xontrib-readable-traceback
      ];
in {
  home = {
    packages = [
      (python.withPackages pythonPkgs)
      pkgs.pipenv
      pkgs.pyenv
      pkgs.pipx
      pkgs.uv
      pkgs.pixi
      (disablePytest pkgs.hatch)
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
