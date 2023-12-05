{ config, pkgs, ... }: [
  "$mainMode, space, exec, ${pkgs.rofi-wayland}/bin/rofi -combi-modi window,drun -show combi -show-icons"
  "$mainMode, V, exec, sh -c '${pkgs.cliphist}/bin/cliphist list | dmenu | cliphist decode | wl-copy'"
  "$mainMode, Enter, exec, ${config.programs.doom-emacs.package}/bin/emacsclient -c"

  "$mainMod, Q, killactive,"
  "$mainMod SHIFT, Q, exit,"
  "$mainMod, F, togglefloating,"
  #"$mainMod, P, pseudo," # dwindle
  "$mainMod, J, togglesplit," # dwindle

  # Move focus with mainMod + arrow keys
  "$mainMod, left, movefocus, l"
  "$mainMod, right, movefocus, r"
  "$mainMod, up, movefocus, u"
  "$mainMod, down, movefocus, d"

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
  "SUPER, L, exec, ${pkgs.wdisplays}/bin/wdisplays"
  # ", XF86RFKill, exec, "
  # ", Print, exec, "
  # ", X86AudioMedia, exec, "
]
