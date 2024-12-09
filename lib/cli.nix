{ pkgs, config, flox, ... }: {
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
        ldns # drill
        inetutils # ping6
        nurl

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
        fastfetch
        nix-du
        pwgen
        xkcdpass
        file
        pciutils
        lshw
        hwloc
        libfaketime
        imagemagick
        ghostscript_headless

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
      tmux = {
        enable = true;
        extraConfig = ''
          set -g prefix C-h
          unbind C-b
          bind C-h send-prefix

          # force a reload of the config file
          unbind r
          bind r source-file ~/.tmux.conf

          # Enable mouse mode (tmux 2.1 and above)
          set -g mouse on

          set -g visual-bell on

          # Split window using | and -
          bind | split-window -h
          bind - split-window -v
          unbind '"'
          unbind %
        '';
      };
    };
  };
}
