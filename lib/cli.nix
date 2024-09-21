{ pkgs, config, nix-index-database, flox, ... }: {
  imports = [
    nix-index-database.hmModules.nix-index
  ];
  config = {
    home = {
      packages = with pkgs; [
        # Utils
        coreutils
        findutils
        gnugrep
        gnused
        gawk
        psmisc
        # TODO: moreutils parallel conflicts with GNU parallel
        ripgrep
        fd
        diffutils
        unixtools.procps
        unixtools.util-linux
        unixtools.xxd
        unixtools.script
        unixtools.watch
        parallel
        hwatch
        watchman

        # Netowrking
        unixtools.route
        unixtools.ping
        unixtools.netstat
        mtr

        # Compressions
        zip
        unzip
        gnutar
        bzip2
        xz
        dtrx

        # System utils
        tmux
        htop
        gdu
        trash-cli
        neofetch
        nix-du
        pwgen
        xkcdpass
        file
        pciutils
        lshw
        hwloc
        libfaketime

        # Sending files
        rclone
        rsync
        wgetpaste
        magic-wormhole

        # Data munging
        sqlite
        jq
        yq
        bc

        # Misc
        mosh
        gnupg
        man-pages
        man-pages-posix
        flox.packages.${system}.flox

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
