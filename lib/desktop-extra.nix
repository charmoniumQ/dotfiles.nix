{ config, pkgs, lib, ... }:
let ignoreVulnerabilities = pkg: pkg.overrideAttrs (super: {
      meta = super.meta // {
        knownVulnerabilities = [];
      };
    });
in {
  config = {
    home = lib.attrsets.optionalAttrs config.desktop.enable {
      packages = with pkgs; [
        # TODO: lshw-gui conflicts with lshw
        pdftk
        gthumb
        distrobox
        yt-dlp

        # Matrix client battle
        # cinny
        # fluffychat
        # element-desktop
        (nheko.override (super: {
          olm = ignoreVulnerabilities super.olm;
          mtxclient = super.mtxclient.override (super: {
            olm = ignoreVulnerabilities super.olm;
          });
        }))

        # Fixes Warning: qt.qpa.plugin: Could not find the Qt platform plugin "wayland" in ""
        libsForQt5.qt5.qtwayland

        kdePackages.discover # Optional: Install if you use Flatpak or fwupd firmware update sevice
        kdePackages.kcalc # Calculator
        kdePackages.ksystemlog # KDE SystemLog Application
        kdiff3 # Compares and merges 2 or 3 files or directories
        kdePackages.isoimagewriter # Optional: Program to write hybrid ISO files onto USB disks
        kdePackages.partitionmanager # Optional Manage the disk devices, partitions and file systems on your computer
        hardinfo2 # System information and benchmarks for Linux systems
      ];
    };
  };
}
