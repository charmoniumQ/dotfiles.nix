{ pkgs, config, flox, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        # Classic utils
        coreutils
        findutils
        gnugrep
        gnused
        gawk
        # TODO: moreutils parallel conflicts with GNU parallel
        diffutils
        unixtools.util-linux
        unixtools.xxd
        unixtools.script
        parallel
        lsof
        watchman # watch a directory for file changes
        shellcheck
        progress
        pv
        sysctl

        # Next gen tools
        ripgrep # grep replacement
        fd # file replacement
        duf # df replacement
        gdu # du replacement
        bat # cat replacement
        grc # cat replacement
        hexyl # hexdump replacement
        hwatch # watch replacement
        sd # sed replacement

        # TUI builders
        rich-cli
        choose

        # Monitoring
        htop # CPU/mem
        btop # everything else
        glances # everything else
        smartmontools
        cpufrequtils
        iotop
        nethogs
        lm_sensors

        # Netowrking
        unixtools.route
        unixtools.ping
        unixtools.netstat
        mtr
        nload
        doggo # drill replacement
        inetutils # ping6

        # Compressions
        zip
        unzip
        gnutar
        bzip2
        xz
        dtrx

        # System utils
        tmux
        trash-cli
        fastfetch
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
        neofetch = "fastfetch";
        rm = "trash";
        ncdu = "gdu";
        edit = "emacsclient";
        l = "lsd --human-readable --almost-all --long --timesort";
        tree = "lsd --human-readable --long --timesort --tree";
        rs = "rsync  --archive --verbose --progress --partial --human-readable";
        df = "duf";
        dig = "echo 'try doggo'";
        drill = "echo 'try doggo'";
        watch = "echo 'try hwatch'";
        pstree = "echo 'try procs --tree'";

        # Shortcuts
        pass = "pwgen --capitalize --numerals --symbols --ambiguous 20 1";
        passphrase = "xkcdpass --wordfile eff-long --numwords 14";
      };
      file = {
        ".ssh" = {
          source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/ssh";
        };
      };
    };
    programs = {
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
