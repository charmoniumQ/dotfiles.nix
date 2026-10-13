{ config, lib, pkgs, ... }@inputs:
let
  lua = lib.generators.mkLuaInline;
  cmds = [
    ["${config.services.dunst.package}/bin/dunst"]
    ["${config.programs.waybar.package}/bin/waybar"]
    ["${pkgs.swaybg}/bin/swaybg" "--mode" "fill" "--image" "${config.desktop.bgimg}"]
    # ["${pkgs.udiskie}/bin/udiskie"]
    ["${pkgs.wl-clipboard}/bin/wl-paste" "--watch" "cliphist" "store"]
    ["${pkgs.networkmanagerapplet}/bin/nm-applet"]
  ] ++ (lib.lists.optionals (config.desktop.guiFramework == "gtk") [
    ["${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"]
  ]) ++ (lib.lists.optionals (config.desktop.guiFramework == "qt") [
    ["${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1"]
  ]);
in {
  on = {
    _args = [
      "hyprland.start"
      (lua ''
        function()
        ${
          lib.concatMapStringsSep
            "\n"
            (cmd: "hl.dsp.exec_raw(\"${lib.strings.join " " cmd}\")")
            cmds
        }
        end
      '')
    ];
  };

  config = {
    general = {
      gaps_in = 2;
      gaps_out = 4;
      border_size = 1;
      col = {
        active_border = {
          colors = [ "rgb(dc8a78)" "rgb(8839ef)" ];
        };
        inactive_border = {
          colors = ["rgb(52679d)" "rgb(4c4f45)"];
        };
      };
    };
    decoration = {
      dim_inactive = true;
      dim_strength = 0.3;
      rounding = 8;
      blur = {
        enabled = false;
      };
      shadow = {
        enabled = false;
      };
      glow = {
        enabled = false;
      };
    };
    animations = {
      enabled = false;
    };
    input = {
      kb_options = "compose:ralt";
      numlock_by_default = true;
      touchpad = {
        natural_scroll = true;
      };
    };
    dwindle = {
      preserve_split = true;
    };
    misc = {
      # https://www.reddit.com/r/hyprland/comments/18af8xi/ghost_anime_girl_when_moving_firefox_windows/
      disable_hyprland_logo = true;
    };
  };

  # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
  gesture = { fingers = 3; direction = "horizontal"; action = "workspace"; };

  bind = (import ./binds.nix) inputs;
}
