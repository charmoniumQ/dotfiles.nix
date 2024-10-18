{ lib, config, pkgs, ... }: {
  home = lib.attrsets.optionalAttrs config.desktop.enable {
    packages = with pkgs; [
      # keepassxc
      # bitwarden
      # bitwarden-cli
      (if config.desktop.guiFramework == "gtk"
       then pkgs.seahorse
       else if config.desktop.guiFramework == "qt"
       then libsForQt5.kwalletmanager
       else builtins.throw "Unknown desktop guiFramework: ${config.desktop.guiFramework}")
    ];
  };
  services = lib.attrsets.optionalAttrs config.desktop.enable {
    gnome-keyring = {
      enable = true;
      components = [ "secrets" ];
    };
  };
}
