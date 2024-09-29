{ lib, config, pkgs, ... }: {
  home = lib.attrsets.optionalAttrs config.desktop.enable {
    packages = with pkgs; [
      libreoffice
    ];
  };
}
