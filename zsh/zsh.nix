pkgs: config: {
  enable = true;
  enableCompletion = true;
  syntaxHighlighting = {
    enable = true;
  };
  enableAutosuggestions = true;
  #historySubstringSearch = {
  #  enable = true;
  #};
  autocd = true;
  dotDir = ".config/zsh";
  initExtra = (builtins.readFile ./initExtra.zsh) + "
    fpath+=(
      ${pkgs.zsh-completions}/share/zsh/site-functions \
      ${pkgs.nix-zsh-completions}/share/zsh/site-functions \
      ${pkgs.starship}/share/zsh/site-functions \
    )
    export PATH=${pkgs.starship}/bin:$PATH
    eval \"\$(starship init zsh)\"
  ";
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
}
