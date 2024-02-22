{ pkgs, config, lib, ... }: {
  services = {
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
  };
  home = {
    packages = with pkgs; [
      nextcloud-client
    ];
  };
  systemd = {
    user = {
      services = {
        nextcloud-client = {
          Service = {
            Environment = lib.mkForce "'PATH=${config.home.profileDirectory}/bin' 'QT_QPA_PLATFORM=wayland'";
          };
        };
      };
    };
  };
}
