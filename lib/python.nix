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
    qtconsole
    ptpython

    hy

    panflute

    # Debugging
    icecream
    ipdb

    # Utilities
    cyclopts
    click
    typer
    tqdm
    pyyaml
    types-pyyaml
    dask
    rich
    textual
    jsonschema
    # TODO: fix macropy
    pytest
    pygithub
    gitpython

    # Scraping
    lxml
    beautifulsoup4
    types-beautifulsoup4
    requests
    types-requests

    # Data science
    numpy
    scipy
    pandas
    scikit-learn
    torch
    jax
    pyarrow
    polars
    pendulum
    scikit-image
    h5py
    xlrd
    networkx

    # Servers
    flask

    # Plotting
    matplotlib
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
  ];
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
