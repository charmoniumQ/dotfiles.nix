{ pkgs, config, ... }:
let
  python = pkgs.python311;
  pythonPkgs = pypkgs: with pypkgs; [
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
    # python-lsp-black
    python-lsp-ruff
  ];
in {
  home = {
    packages = [
      (python.withPackages pythonPkgs)
      pkgs.ruff
      pkgs.poetry
    ];
  };
}
