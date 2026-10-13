{ config, lib, pkgs, ... }:
let
  lua = lib.generators.mkLuaInline;
  bind = keys: dispatch: { _args = [ keys dispatch ]; };
  exec = cmd: lua ''hl.dsp.exec_raw("${lib.strings.join " " cmd}")'';
  focus = direction: lua ''hl.dsp.focus({ direction = "${direction}" })'';
  switchTo = ws: lua ''hl.dsp.focus({ workspace = ${builtins.toJSON ws} })'';
  moveTo = ws: lua ''hl.dsp.window.move({ workspace = ${builtins.toString  ws} })'';
  moveToMonitorDir = direction: lua ''hl.dsp.workspace.move({ workspace = "+0"; monitor = "${direction}"; })'';
  moveToMonitor = ws: lua ''function() hl.dispatch(hl.dsp.workspace.move({ workspace = "+0"; monitor = hl.get_monitor(${builtins.toString ws}); })) end'';
in [
  (bind "SUPER + space" (exec ["${pkgs.rofi}/bin/rofi" "-combi-modi" "window,drun" "-show" "combi" "-show-icons"]))
  # TODO: cliphist
  (bind "SUPER + V" (exec ["${pkgs.cliphist}/bin/cliphist list | ${pkgs.rofi}/bin/rofi -dmenu | ${pkgs.cliphist}/bin/cliphist decode | ${pkgs.wl-clipboard}/bin/wl-copy"]))
  (bind "SUPER + L" (exec ["lock"]))

  (bind "SUPER + Q" (lua "hl.dsp.window.kill()"))
  (bind "SUPER + SHIFT + Q" (lua "hl.dsp.exit()"))
  (bind "SUPER + F" (lua "hl.dsp.window.fullscreen()"))
  (bind "SUPER + G" (lua ''hl.dsp.window.float({ action = "toggle" })''))
  #(bind "SUPER + P" (lua "hl.dsp.window.pseudo()")) # dwindle
  #(bind "SUPER + J" (lua ''hl.dsp.layout"togglesplit")'')) # dwindle

  # Move focus with super + arrow keys
  (bind "SUPER + left" (focus "left"))
  (bind "SUPER + right" (focus "right"))
  (bind "SUPER + up" (focus "up"))
  (bind "SUPER + down" (focus "down"))

  (bind "SUPER + SHIFT + left" (moveToMonitorDir "left"))
  (bind "SUPER + SHIFT + right" (moveToMonitorDir "right"))
  (bind "SUPER + SHIFT + up" (moveToMonitorDir "up"))
  (bind "SUPER + SHIFT + down" (moveToMonitorDir "down"))

  # Switch workspaces with super + [0-9]
  (bind "SUPER + 1" (switchTo 1))
  (bind "SUPER + 2" (switchTo 2))
  (bind "SUPER + 3" (switchTo 3))
  (bind "SUPER + 4" (switchTo 4))
  (bind "SUPER + 5" (switchTo 5))
  (bind "SUPER + 6" (switchTo 6))
  (bind "SUPER + 7" (switchTo 7))
  (bind "SUPER + 8" (switchTo 8))
  (bind "SUPER + 9" (switchTo 9))
  (bind "SUPER + 0" (switchTo 10))

  # Move active window to a workspace with super + SHIFT + [0-9]
  (bind "SUPER + SHIFT + 1" (moveTo 1))
  (bind "SUPER + SHIFT + 2" (moveTo 2))
  (bind "SUPER + SHIFT + 3" (moveTo 3))
  (bind "SUPER + SHIFT + 4" (moveTo 4))
  (bind "SUPER + SHIFT + 5" (moveTo 5))
  (bind "SUPER + SHIFT + 6" (moveTo 6))
  (bind "SUPER + SHIFT + 7" (moveTo 7))
  (bind "SUPER + SHIFT + 8" (moveTo 8))
  (bind "SUPER + SHIFT + 9" (moveTo 9))
  (bind "SUPER + SHIFT + 0" (moveTo 10))

  # Move workspace to current display with super + CTRL + [0-9]
  (bind "SUPER + CTRL + 1" (moveToMonitor 1))
  (bind "SUPER + CTRL + 2" (moveToMonitor 2))
  (bind "SUPER + CTRL + 3" (moveToMonitor 3))
  (bind "SUPER + CTRL + 4" (moveToMonitor 4))
  (bind "SUPER + CTRL + 5" (moveToMonitor 5))
  (bind "SUPER + CTRL + 6" (moveToMonitor 6))
  (bind "SUPER + CTRL + 7" (moveToMonitor 7))
  (bind "SUPER + CTRL + 8" (moveToMonitor 8))
  (bind "SUPER + CTRL + 9" (moveToMonitor 9))
  (bind "SUPER + CTRL + 0" (moveToMonitor 10))

  # Scroll through existing workspaces with super + scroll
  (bind "SUPER + mouse_down" (switchTo "+1"))
  (bind "SUPER + mouse_up" (switchTo "-1"))

  (bind "XF86AudioMute" (exec ["${pkgs.pulseaudio}/bin/pactl" "set-sink-mute" "@DEFAULT_SINK@" "toggle"]))
  (bind "XF86AudioPrev" (exec ["${pkgs.pulseaudio}/bin/pactl" "set-sink-volume" "@DEFAULT_SINK@" "-5%"]))
  (bind "XF86AudioRaiseVolume" (exec ["${pkgs.pulseaudio}/bin/pactl" "set-sink-volume" "@DEFAULT_SINK@" "+5%"]))
  (bind "SUPER + XF86AudioMute" (exec ["${pkgs.pavucontrol}/bin/pavucontrol"]))
  # (bind "XF86AudioPrev" (exec ""))
  # (bind "XF86AudioPlay" (exec ""))
  # (bind "XF86AudioNext" (exec ""))
  (bind "XF86MonBrightnessDown" (exec ["${pkgs.brightnessctl}/bin/brightnessctl" "set" "1%-"]))
  (bind "XF86MonBrightnessUp" (exec ["${pkgs.brightnessctl}/bin/brightnessctl" "set" "1%+"]))
  (bind "SHIFT + XF86MonBrightnessDown" (exec ["${pkgs.brightnessctl}/bin/brightnessctl" "set" "10%-"]))
  (bind "SHIFT + XF86MonBrightnessUp" (exec ["${pkgs.brightnessctl}/bin/brightnessctl" "set" "10%+"]))
  (bind "SUPER + P" (exec ["${pkgs.nwg-displays}/bin/wdisplays"]))
  # (bind "XF86RFKill" (exec ""))
  (bind "Print" (exec ["${pkgs.hyprshot}/bin/hyprshot --raw --mode window | ${pkgs.swappy}/bin/swappy --file -"]))
  (bind "SHIFT + Print" (exec ["${pkgs.hyprshot}/bin/hyprshot --raw --mode region | ${pkgs.swappy}/bin/swappy --file -"]))
  # (bind "X86AudioMedia" (exec ""))

  {
    _args = ["SUPER + mouse:272" (lua "hl.dsp.window.drag()") { mouse = true; }];
  }
  {
    _args = ["SUPER + CTRL + mouse:272" (lua "hl.dsp.window.resize()") { mouse = true; }];
  }
]
