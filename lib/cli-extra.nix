{ pkgs, config, ... }: let
  disableTests = pkg: pkg.overrideAttrs (old: {
    doCheck = false; 
  });
in {
  # TODO: Use rbw as password store
  # https://github.com/doy/rbw/issues/224
  # https://www.menardo.net/posts/linux/use-vaultwarden-in-gnome-keyring-and-use/
  # https://github.com/bilelmoussaoui/oo7
  # https://github.com/grimsteel/pass-secret-service
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

        # Scanning
        # TODO: re-enable
        (disableTests gscan2pdf)

        # Next gen tools
        duf # df replacement
        bat # cat replacement
        grc # cat replacement
        hexyl # hexdump replacement
        hwatch # watch replacement

        # Monitoring
        btop # everything else
        smartmontools
        cpufrequtils
        iotop
        nethogs
        lm_sensors
        rbw
        pinentry-gtk2

        # System utils
        pciutils
        lshw
        hwloc
        ghostscript_headless
        libfaketime

        # Sending files
        rclone
      ];
      shellAliases = {
        df = "duf";
        xxd = "hexyl";
        watch = "hwatch";
      };
    };
  };
}
