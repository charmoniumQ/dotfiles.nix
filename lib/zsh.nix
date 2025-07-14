{ pkgs, config, ... }:
let
  addToFpath = pkgs: "fpath+=(" + (builtins.concatStringsSep " " (builtins.map (pkg: "${pkg}/share/zsh/site-functions") pkgs)) + ")";
in 
{
  home = {
    packages = [ pkgs.starship ];
  };
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      syntaxHighlighting = {
        enable = true;
      };
      autosuggestion = {
        enable = true;
      };
      #historySubstringSearch = {
      #  enable = true;
      #};
      autocd = true;
      dotDir = ".config/zsh";
      initContent = builtins.concatStringsSep "\n" [
        (addToFpath [
          pkgs.zsh-completions
          pkgs.nix-zsh-completions
          pkgs.starship
        ])
        ''eval "$(starship init zsh)"''
        (builtins.readFile ./zsh/initExtra.zsh)
      ];
      sessionVariables = {
        # This gives us a hook to prepend to PS1 especiallhttps://github.com/direnv/direnv/wiki/PS1 in direnv.
        # https://github.com/direnv/direnv/wiki/PS1
        DISABLE_UNTRACKED_FILES_DIRTY = "true";
        COMPLETION_WAITING_DOTS = "true";
      };
    };
  };
}
