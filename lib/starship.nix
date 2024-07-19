{ ... }: {
  programs = {
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
          bash_indicator = "bash";
          zsh_indicator = "zsh";
          xonsh_indicator = "xonsh";
        };
      };
    };
  };
}
