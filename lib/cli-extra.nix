{ pkgs, config, flox, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        # Classic utils
        diffutils
        unixtools.util-linux
        unixtools.xxd
        unixtools.script
        # TODO: moreutils parallel conflicts with GNU parallel
        parallel
        watchman # watch a directory for file changes
        progress

        # Monitoring
        btop # everything else
        smartmontools
        cpufrequtils
        iotop
        nethogs
        lm_sensors

        # System utils
        pciutils
        lshw
        hwloc
        ghostscript_headless
        libfaketime

        # Sending files
        rclone
      ];
    };
  };
}
