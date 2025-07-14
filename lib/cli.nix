{ pkgs, config, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        # Classic utils
        coreutils
        findutils
        gnugrep
        gnused
        gawk
        pv
        lsof
        progress
        sysctl

        # Next gen tools
        ripgrep # grep replacement
        fd # file replacement
        gdu # du replacement
        sd # sed replacement
        procs # pstree, ps replacement

        # TUI builders
        rich-cli
        gum

        # Monitoring
        htop # CPU/mem
        glances # everything else

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
        imagemagick

        # Sending files
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
        dig = "doggo";
        drill = "doggo";
        pstree = "procs --tree";

        # Shortcuts
        pass = "pwgen --capitalize --numerals --symbols --ambiguous 20 1";
        passphrase = "xkcdpass --wordfile eff-long --numwords 14";
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
