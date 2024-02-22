{ config, pkgs, ... }: [
  "$mainMode, space, exec, ${pkgs.rofi-wayland}/bin/rofi -combi-modi window,drun -show combi -show-icons"
  "$mainMode, V, exec, ${pkgs.cliphist}/bin/cliphist list | ${pkgs.rofi-wayland}/bin/rofi -dmenu | ${pkgs.cliphist}/bin/cliphist decode | ${pkgs.wl-clipboard}/bin/wl-copy"
  "$mainMode, Enter, exec, ${config.programs.doom-emacs.package}/bin/emacsclient -c"
  "$mainMode, L, exec, lock"

  "$mainMod, Q, killactive,"
  "$mainMod SHIFT, Q, exit,"
  "$mainMod, F, fullscreen,"
  "$mainMod, G, togglefloating,"
  #"$mainMod, P, pseudo," # dwindle
  #"$mainMod, J, togglesplit," # dwindle

  # Move focus with mainMod + arrow keys
  "$mainMod, left, movefocus, l"
  "$mainMod, right, movefocus, r"
  "$mainMod, up, movefocus, u"
  "$mainMod, down, movefocus, d"

  "$mainMod, J, exec, ${pkgs.hyprnome}/bin/hyprnome --previous"
  "$mainMod, K, exec, ${pkgs.hyprnome}/bin/hyprnome"
  "$mainMod SHIFT, J, exec, ${pkgs.hyprnome}/bin/hyprnome --previous --move"
  "$mainMod SHIFT, K, exec, ${pkgs.hyprnome}/bin/hyprnome --move"

  # Switch workspaces with mainMod + [0-9]
  "$mainMod, 1, workspace, 1"
  "$mainMod, 2, workspace, 2"
  "$mainMod, 3, workspace, 3"
  "$mainMod, 4, workspace, 4"
  "$mainMod, 5, workspace, 5"
  "$mainMod, 6, workspace, 6"
  "$mainMod, 7, workspace, 7"
  "$mainMod, 8, workspace, 8"
  "$mainMod, 9, workspace, 9"
  "$mainMod, 0, workspace, 10"

  # Move active window to a workspace with mainMod + SHIFT + [0-9]
  "$mainMod SHIFT, 1, movetoworkspace, 1"
  "$mainMod SHIFT, 2, movetoworkspace, 2"
  "$mainMod SHIFT, 3, movetoworkspace, 3"
  "$mainMod SHIFT, 4, movetoworkspace, 4"
  "$mainMod SHIFT, 5, movetoworkspace, 5"
  "$mainMod SHIFT, 6, movetoworkspace, 6"
  "$mainMod SHIFT, 7, movetoworkspace, 7"
  "$mainMod SHIFT, 8, movetoworkspace, 8"
  "$mainMod SHIFT, 9, movetoworkspace, 9"
  "$mainMod SHIFT, 0, movetoworkspace, 10"

  # Move workspace to current display with mainMod + CTRL + [0-9]
  "$mainMod CTRL, 1, exec, hyprctl dispatch moveworkspacetomonitor 1 current ; hyprctl dispatch workspace 1"
  "$mainMod CTRL, 2, exec, hyprctl dispatch moveworkspacetomonitor 2 current ; hyprctl dispatch workspace 2"
  "$mainMod CTRL, 3, exec, hyprctl dispatch moveworkspacetomonitor 3 current ; hyprctl dispatch workspace 3"
  "$mainMod CTRL, 4, exec, hyprctl dispatch moveworkspacetomonitor 4 current ; hyprctl dispatch workspace 4"
  "$mainMod CTRL, 5, exec, hyprctl dispatch moveworkspacetomonitor 5 current ; hyprctl dispatch workspace 5"
  "$mainMod CTRL, 6, exec, hyprctl dispatch moveworkspacetomonitor 6 current ; hyprctl dispatch workspace 6"
  "$mainMod CTRL, 7, exec, hyprctl dispatch moveworkspacetomonitor 7 current ; hyprctl dispatch workspace 7"
  "$mainMod CTRL, 8, exec, hyprctl dispatch moveworkspacetomonitor 8 current ; hyprctl dispatch workspace 8"
  "$mainMod CTRL, 9, exec, hyprctl dispatch moveworkspacetomonitor 9 current ; hyprctl dispatch workspace 9"
  "$mainMod CTRL, 0, exec, hyprctl dispatch moveworkspacetomonitor 10 current ; hyprctl dispatch workspace 10"

  # Example special workspace (scratchpad)
  "$mainMod, S, togglespecialworkspace, magic"
  "$mainMod SHIFT, S, movetoworkspace, special:magic"

  # Scroll through existing workspaces with mainMod + scroll
  "$mainMod, mouse_down, workspace, e+1"
  "$mainMod, mouse_up, workspace, e-1"

  ", XF86AudioMute, exec, ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle"
  ", XF86AudioLowerVolume, exec, ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%"
  ", XF86AudioRaiseVolume, exec, ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%"
  "$mainMod, XF86AudioMute, exec, ${pkgs.pavucontrol}/bin/pavucontrol"
  # ", XF86AudioPrev, exec, "
  # ", XF86AudioPlay, exec, "
  # ", XF86AudioNext, exec, "
  ", XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 1%-"
  ", XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 1%+"
  "Shift, XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 10%-"
  "Shift, XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 10%+"
  "SUPER, P, exec, ${pkgs.wdisplays}/bin/wdisplays"
  # ", XF86RFKill, exec, "
  ", Print, exec, ${pkgs.hyprshot}/bin/hyprshot --raw --mode window | ${pkgs.swappy}/bin/swappy --file -"
  "Shift, Print, exec, ${pkgs.hyprshot}/bin/hyprshot --raw --mode region | ${pkgs.swappy}/bin/swappy --file -"
  # ", X86AudioMedia, exec, "
]
