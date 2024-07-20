{ pkgs, config, nix-index-database, flox, ... }: {
  imports = [
    nix-index-database.hmModules.nix-index
  ];
  config = {
    home = {
      packages = with pkgs; [
        zip
        unzip
        gnutar
        bzip2
        xz
        tmux
        htop
        direnv
        gdu
        trash-cli
        rclone
        rsync
        neofetch
        dtrx
        mosh
        psmisc
        sqlite
        moreutils
        wgetpaste
        ripgrep
        jq
        yq
        fd
        gnupg
        magic-wormhole
        man-pages
        man-pages-posix
        flox.packages.${system}.flox
        nix-du
        mtr
        pwgen
        xkcdpass
        file
        mosh
        (pkgs.stdenv.mkDerivation {
          name = "scripts";
          src = ./scripts;
          installPhase = ''
          install --directory $out/bin
          install --target-directory=$out/bin *
        '';
        })
      ];
      shellAliases = {
        # Newer tools
        rm = "trash";
        ncdu = "gdu";
        edit = "emacsclient";
        l = "lsd --human-readable --almost-all --long --timesort";
        tree = "lsd --human-readable --long --timesort --tree";
        rs = "rsync  --archive --verbose --progress --partial --human-readable";
        nix-build = "nom-build";
        nix-shell = "nom-shell";

        # Shortcuts
        pass = "pwgen --capitalize --numerals --symbols --ambiguous 20 1";
        passphrase = "xkcdpass --wordfile eff-long --numwords 14";
        nix-locat = "nix-locate --top-level --regex";
      };
      file = {
        # ".ssh" = {
        #   source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/ssh";
        # };
      };
    };
    programs = {
      nix-index = {
        enable = true;
      };
      nix-index-database = {
        comma = {
          enable = true;
        };
      };
      lsd = {
        enable = true;
      };
      bash = {
        enable = true;
      };
      man = {
        enable = true;
      };
    };
  };
}
