{ lib, pkgs, config, ... }: {
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
        choose # cut replacement

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
        httpie
        curl

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
        gcr # for gpg-agent pinentry

        # Misc
        mosh
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

        # Default configs
        mvn = "mvn -gs $XDG_CONFIG_HOME/maven/settings.xml";

        # Shortcuts
        pass = "pwgen --capitalize --numerals --symbols --ambiguous 20 1";
        passphrase = "xkcdpass --wordfile eff-long --numwords 14";
      };
    };
    programs = {
      lsd = {
        enable = true;
      };
      gpg = {
        enable = true;
        homedir = "${config.xdg.dataHome}/gnupg";
        mutableKeys = true;
        mutableTrust = true;
        settings = {
          keyserver = [
            "hkps://pgpkeys.eu"
            "hkps://keys.openpgp.org"
          ];
          # auto-key-locate = "local,wkd,dane,keyserver";
        };
        # TODO: import trusted pub keys
      };
      bash = {
        enable = true;
        bashrcExtra = lib.optionalString config.targets.genericLinux.enable ''
          profile=$HOME/.local/state/nix/profile
          if [ -d $profile ]; then
            source $profile/etc/profile.d/nix.sh
          fi
          export HISTFILE="''${XDG_STATE_HOME:$HOME/.local/state}/bash/history";
        '';
      };
      zsh = {
        enable = true;
        initContent = lib.optionalString config.targets.genericLinux.enable ''
          profile=$HOME/.local/state/nix/profile
          if [ -d $profile ]; then
            source $profile/etc/profile.d/nix.sh
          fi
          export HISTFILE="''${XDG_STATE_HOME:$HOME/.local/state}/zsh/history";
        '';
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
    services = {
      gpg-agent = {
        enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;
        enableSshSupport = true;
        pinentry = {
           package = pkgs.pinentry-gnome3;
        };
      };
    };
  };
}
