{ pkgs, config, lib, ... }: {
  services = lib.attrsets.optionalAttrs config.desktop.enable {
    nextcloud-client = {
      enable = false;
      startInBackground = false;
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
          Unit = {
            Description = "Sync Nextcloud";
            After = [ "network-online.target" ];
          };
          Service = {
            Type = "oneshot";
            Environment = "PATH=" + (lib.strings.makeBinPath [ pkgs.nextcloud-client pkgs.gnugrep ]) + " HOME=/home/sam";
            ExecStart = "${pkgs.bash}/bin/bash -c '
              nextcloudcmd --exclude $HOME/box/.ignore -n $HOME/box/ https://nextcloud.samgrayson.me 2>&1 \
                | grep -v CSyncEnums::CSYNC_INSTRUCTION_IGNORE \
                | grep -v OCC::SyncFileItem::FileIgnored \
                | grep -v OCC::ProcessDirectoryJob::NormalQuery \
                | grep -v CSyncEnums::ItemTypeFile \
                | grep -v OCC::SyncFileItem::None \
              ;'";
            Nice = 10;  # Lower priority
            CPUQuota = "50%";
            IOSchedulingClass = "idle";
          };
          Install = {
            WantedBy = [ "default.target" ];
          };
        };
      };
      timers = {
        nextcloud-client-timer = {
          Timer = {
            OnBootSec = "5min";
            OnUnitActiveSec = "6h";
            Persistent = true;
          };
          Install = {
            WantedBy = [ "timers.target" ];
          };
        };
      };
    };
  };
}
