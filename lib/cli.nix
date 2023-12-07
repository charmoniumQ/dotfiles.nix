{ pkgs, config, ... }: {
  home = {
    packages = with pkgs; [
      comma
      tmux
      htop
      direnv
      fcp
      gdu
      trash-cli
      rclone
      rsync
      neofetch
      dtrx
      mosh
      psmisc
      moreutils
      wgetpaste
      ripgrep
      jq
      yq
      fd
      gnupg
      bat
      magic-wormhole
      nix-du
      mtr
      pwgen
      xkcdpass
      nix-index
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
      cat = "bat";
      ncdu = "gdu";
      l = "lsd --human-readable --almost-all --long --timesort";
      ls = "lsd";
      cp = "fcp";
      tree = "lsd --human-readable --long --timesort --tree";
      rs = "rsync  --archive --verbose --progress --partial --human-readable";

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
  };
}
