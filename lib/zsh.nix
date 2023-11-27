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
      enableAutosuggestions = true;
      #historySubstringSearch = {
      #  enable = true;
      #};
      autocd = true;
      dotDir = ".config/zsh";
      initExtra = builtins.concatStringsSep "\n" [
        (builtins.readFile ./zsh/initExtra.zsh)
        (addToFpath [pkgs.zsh-completions pkgs.nix-zsh-completions pkgs.starship])
        ''eval "$(starship init zsh)"''
      ];
      sessionVariables = {
        # This gives us a hook to prepend to PS1 especiallhttps://github.com/direnv/direnv/wiki/PS1 in direnv.
        # https://github.com/direnv/direnv/wiki/PS1
        DISABLE_UNTRACKED_FILES_DIRTY = "true";
        COMPLETION_WAITING_DOTS = "true";
      };
    };
    starship = {
      enable = true;
      settings = {
        gcloud = {
          disabled = true;
        };
        character = {
          success_symbol = "[@](bold green)";
          error_symbol = "[@](bold red)";
        };
        shlvl = {
          disabled = false;
          format = "[$symbol]($style) ";
          repeat = true;
          symbol = "🐚";
          repeat_offset = 1;
          threshold = 0;
        };
        shell = {
          disabled = false;
          bash_indicator = "🅱️";
          zsh_indicator = "💤";
          xonsh_indicator = "xonsh";
        };
      };
    };
  };
}
