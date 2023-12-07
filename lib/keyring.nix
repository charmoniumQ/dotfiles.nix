{ config, pkgs, ... }: {
  home = {
    packages = with pkgs; [
      keepassxc
      bitwarden
      (if config.desktop.guiFramework == "gtk"
       then gnome.seahorse
       else if config.desktop.guiFramework == "qt"
       then libsForQt5.kwalletmanager
       else builtins.throw "Unknown desktop guiFramework: ${config.desktop.guiFramework}")
    ];
  };
  services = {
    gnome-keyring = {
      enable = true;
      components = [ "secrets" ];
    };
  };
}
