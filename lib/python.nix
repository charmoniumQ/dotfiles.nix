{ pkgs, config, ... }:
let
  python = pkgs.python312;
  pythonPkgs = pypkgs: with pypkgs; [
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

    # Files
    aiofiles
    fsspec
    sshfs

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
    fabric

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
  ] ++ (lib.lists.optional config.desktop.enable qtconsole);
in {
  home = {
    packages = [
      (python.withPackages pythonPkgs)
      pkgs.ruff
      pkgs.poetry
      # TODO: fix pkgs.pipenv
      pkgs.pipx
      pkgs.hatch
      pkgs.virtualenv
    ];
  };
}
