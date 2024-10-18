{ pkgs, config, ... }:
{
  home = {
    file = {
      xonsh = {
        source = ./xonsh/rc.xsh;
        target = ".config/xonsh/rc.xsh";
      };
      xonsh-aliases = {
        text = builtins.toJSON config.home.shellAliases;
        target = ".config/xonsh/aliases.json";
      };
    };
    packages = [
      pkgs.starship
    ];
  };
  programs = {
    fzf = {
      enable = true;
    };
    zoxide = {
      enable = true;
      options = [ "--hook" "pwd" ];
      # TODO: zoxide.el
      # TODO: Xonsh direnv
    };
  };
}
