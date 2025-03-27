{ pkgs, lib, config, ... }: {
  services = lib.attrsets.optionalAttrs config.desktop.enable {
    udiskie = {
      enable = true;
      notify = true;
      tray = "always";
    };
  };
  home = {
    packages = with pkgs; [
      parted
      e2fsprogs
      dosfstools
      fuse
    ];
  };
}
