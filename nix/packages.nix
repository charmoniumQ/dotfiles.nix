pkgs: gui: [
  pkgs.tmux
  pkgs.git
  pkgs.zsh
  pkgs.exa
  pkgs.bpytop
  pkgs.htop
  pkgs.glances
  pkgs.direnv
  pkgs.dtrx
  pkgs.ncdu
  pkgs.trash-cli
  pkgs.google-cloud-sdk
  pkgs.rclone
  pkgs.asciinema
  pkgs.mosh
  pkgs.moreutils
  pkgs.wgetpaste
  pkgs.xclip
  pkgs.pdftk
  pkgs.jq
  pkgs.yq
  pkgs.fd
  pkgs.tig
  pkgs.icdiff
  pkgs.rsync
  pkgs.bfg-repo-cleaner
  pkgs.ripgrep
  pkgs.pandoc
  pkgs.ruby_2_7
  pkgs.gnupg
  pkgs.bat
  pkgs.tree
  pkgs.pipenv
  pkgs.poetry
  pkgs.nodejs
  pkgs.nodePackages.npm
  pkgs.magic-wormhole
  pkgs.nix-du
  pkgs.graphviz
  pkgs.mtr
  pkgs.pwgen
  pkgs.xkcdpass
  pkgs.nix-index
  (pkgs.python39.withPackages (ps: with ps; [
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
    pipx
    rich
    pip
  ]))
  (pkgs.stdenv.mkDerivation {
    name = "scripts";
    src = ./../scripts;
    installPhase = ''
      chmod +x *
      mkdir -p $out/bin
      cp * $out/bin/
    '';
  })
] ++ (if gui then [
  # Only packages which do not require GPU acceleration
  pkgs.gitg
  pkgs.meld
  pkgs.xdot
] else [])
