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
            Type = "simple";
            Environment = "PATH=" + (lib.strings.makeBinPath [ pkgs.nextcloud-client pkgs.gnugrep  pkgs.coreutils ]) + " HOME=/home/sam";
            # These are examples of log lines I want to ignore
            # 03-27 13:36:27:913 [ info nextcloud.sync.discovery ]:	"Processing \"SIG_GLUG\" | (db/local/remote) | valid: true/true/true | mtime: 1742847368/1717525986/1742847368 | size: 0/406/0 | etag: \"67e1bd887c698\"//\"67e1bd887c698\" | checksum: \"\"//\"\" | perm: \"DNVCKR\"//\"DNVCKR\" | fileid: \"00000092ocato7fkq0k7\"//\"00000092ocato7fkq0k7\" | type: CSyncEnums::ItemTypeDirectory/CSyncEnums::ItemTypeDirectory/CSyncEnums::ItemTypeDirectory | e2ee: false/false | e2eeMangledName: \"\"/\"\" | file lock: not locked//not locked | file lock type: \"\"//\"0\" | live photo: false//false | metadata missing: /false/"
            # 03-27 13:37:17:082 [ info nextcloud.sync.propagator ]:	Starting CSyncEnums::CSYNC_INSTRUCTION_IGNORE propagation of "ta-fa24-cs527/repos/yichen41/.git" by OCC::PropagateIgnoreJob(0x56325b34fb10)
            # 03-27 13:37:17:082 [ warning nextcloud.sync.propagator ]:	Could not complete propagation of "ta-fa24-cs527/repos/yichen41/.git" by OCC::PropagateIgnoreJob(0x56325b34fb10) with status OCC::SyncFileItem::FileIgnored and error: "File is listed on the ignore list."
            ExecStart =
              let script = builtins.warn "Parameterize this for other users" (pkgs.writeShellScriptBin "script" ''
                set -x +e
                env
                nextcloudcmd --non-interactive --exclude $HOME/box/.sync-exclude.lst -n $HOME/box/ https://nextcloud.samgrayson.me 2>&1 \
                | grep -v nextcloud.sync.discovery \
                | grep -v CSyncEnums::CSYNC_INSTRUCTION_IGNORE \
                | grep -v OCC::SyncFileItem::FileIgnored \
                ;
                exit 0
              '');
              in
                "${script}/bin/script";
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
        nextcloud-client = {
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
