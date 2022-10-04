pkgs: config: {
  enable = true;
  enableCompletion = true;
  autocd = true;
  dotDir = ".config/zsh";
  initExtra = (builtins.readFile ./initExtra.zsh);
  sessionVariables = {
    # This gives us a hook to prepend to PS1 especiallhttps://github.com/direnv/direnv/wiki/PS1 in direnv.
    # https://github.com/direnv/direnv/wiki/PS1
    DISABLE_UNTRACKED_FILES_DIRTY = "true";
    COMPLETION_WAITING_DOTS = "true";
  };
  shellAliases = {
    l = "exa -alrs modified";
    ipy = "ipython";
    py = "python";
    pass = "pwgen --capitalize --numerals --symbols --ambiguous 20 1";
    passphrase = "xkcdpass --wordfile eff-long --numwords 14";
  };
  plugins = [
    {
      name = "zsh-syntax-highlighting";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "0.7.0";
        sha256 = "0s1z3whzwli5452h2yzjzzj27pf1hd45g223yv0v6hgrip9f853r";
      };
    }
    {
      name = "zsh-autosuggestions";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-autosuggestions";
        rev = "v0.6.4";
        sha256 = "0h52p2waggzfshvy1wvhj4hf06fmzd44bv6j18k3l9rcx6aixzn6";
      };
    }
    {
      name = "spaceship-prompt";
      src = pkgs.fetchFromGitHub {
        owner = "denysdovhan";
        repo = "spaceship-prompt";
        rev = "v3.11.2";
        sha256 = "1q7m9mmg82n4fddfz01y95d5n34xnzhrnn1lli0vih39sgmzim9b";
      };
    }
  ];
}
