{ config, lib, pkgs, ... }:
let
  # Common unit settings for long-running session daemons.
  mkService = description: execStart: {
    Unit = {
      Description = description;
      ConditionEnvironment = "HYPRLAND_INSTANCE_SIGNATURE";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = execStart;
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
in {
  config = lib.mkIf config.desktop.enable {
    systemd.user.services = {
      hyprland-dunst = mkService "Notification daemon"
        "${config.services.dunst.package}/bin/dunst";

      hyprland-swaybg = mkService "Wallpaper"
        "${pkgs.swaybg}/bin/swaybg --mode fill --image ${config.desktop.bgimg}";

      # `cliphist` is resolved from PATH by wl-paste.
      hyprland-cliphist = mkService "Clipboard history"
        "${pkgs.wl-clipboard}/bin/wl-paste --watch cliphist store";

      hyprland-nm-applet = mkService "Network Manager applet"
        "${pkgs.networkmanagerapplet}/bin/nm-applet";
    } // (lib.optionalAttrs (config.desktop.guiFramework == "gtk") {
      hyprland-polkit-gnome = mkService "PolicyKit authentication agent (GNOME)"
        "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    }) // (lib.optionalAttrs (config.desktop.guiFramework == "qt") {
      hyprland-polkit-kde = mkService "PolicyKit authentication agent (KDE)"
        "${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1";
    });
  };
}
