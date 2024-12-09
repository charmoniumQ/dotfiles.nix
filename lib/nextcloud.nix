{ pkgs, config, lib, ... }: {
  services = lib.attrsets.optionalAttrs config.desktop.enable {
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
  };
  home = lib.attrsets.optionalAttrs config.desktop.enable {
    packages = with pkgs; [
      nextcloud-client
    ];
  };
  systemd = lib.attrsets.optionalAttrs config.desktop.enable {
    user = {
      services = {
        nextcloud-client = {
          Service = {
            #Environment = lib.mkForce "'PATH=${config.home.profileDirectory}/bin' 'QT_QPA_PLATFORM=wayland'";
          };
        };
      };
    };
  };
}
