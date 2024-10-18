{ config, lib, pkgs, ... }@inputs: {
  # For all categories, see https://wiki.hyprland.org/Configuring/Variables/

  #"debug:disable_logs" = false;

  exec-once = [
    "${config.services.dunst.package}/bin/dunst &"
    "${config.programs.waybar.package}/bin/waybar"
    "${pkgs.swaybg}/bin/swaybg --mode fill --image ${config.desktop.bgimg} &"
    # "${pkgs.udiskie}/bin/udiskie &"
    "${pkgs.wl-clipboard}/bin/wl-paste --watch cliphist store"
    "${pkgs.networkmanagerapplet}/bin/nm-applet &"
  ] ++ (lib.lists.optionals (config.desktop.guiFramework == "gtk") [
    "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1 &"
  ]) ++ (lib.lists.optionals (config.desktop.guiFramework == "qt")
    [
      "${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1 &"
    ]);
  bind = (import ./binds.nix) inputs;
  env = "XCURSOR_SIZE,24";
  input = {
    kb_layout = "us";
    kb_options = "compose:ralt";
    touchpad = {
      natural_scroll = true;
    };
    sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
  };
  source = "~/.config/hypr/monitors.conf";
  general = {
    # See https://wiki.hyprland.org/Configuring/Variables/ for more
    gaps_in = 2;
    gaps_out = 4;
    border_size = 1;
    "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
    "col.inactive_border" = "rgba(595959aa)";
    layout = "dwindle";
    # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
    allow_tearing = false;
  };
  decoration = {
    # See https://wiki.hyprland.org/Configuring/Variables/ for more
    rounding = 5;
    blur = {
      enabled = true;
      size = 3;
      passes = 1;
    };
    drop_shadow = true;
    shadow_range = 4;
    shadow_render_power = 3;
    "col.shadow" = "rgba(1a1a1aee)";
  };
  animations = {
    enabled = true;
    # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
    bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
    animation = [
      "windows, 1, 7, myBezier"
      "windowsOut, 1, 7, default, popin 80%"
      "border, 1, 10, default"
      "borderangle, 1, 8, default"
      "fade, 1, 7, default"
      "workspaces, 1, 6, default"
    ];
  };
  dwindle = {
    # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
    pseudotile = true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
    preserve_split = true; # you probably want this
  };
  master = {
    # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
  };
  gestures = {
    # See https://wiki.hyprland.org/Configuring/Variables/ for more
    workspace_swipe = true;
  };
  misc = {
    # See https://wiki.hyprland.org/Configuring/Variables/ for more
    force_default_wallpaper = 0; # Set to 0 to disable the anime mascot wallpapers
    # https://www.reddit.com/r/hyprland/comments/18af8xi/ghost_anime_girl_when_moving_firefox_windows/
    disable_hyprland_logo = true;
  };
  # Example per-device config
  # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
  # device:epic-mouse-v1 = {
  #     sensitivity = -0.5;
  # };
  #
  # Example windowrule v1
  # windowrule = float, ^(kitty)$
  #
  # Example windowrule v2
  # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
  # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
  #
  # See https://wiki.hyprland.org/Configuring/Keywords/ for more
  "$mainMod" = "SUPER";
  #
  # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
  # Move/resize windows with mainMod + LMB/RMB and dragging
  bindm = [
    "$mainMod, mouse:272, movewindow"
    "$mainMod, mouse:273, resizewindow"
  ];
}
