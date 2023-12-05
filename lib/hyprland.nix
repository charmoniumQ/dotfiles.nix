{ pkgs, config, lib, ... }@inputs: {
  wayland = {
    windowManager = {
      hyprland = {
        enable = true;
        settings = {
          # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
          exec-once = [
            "${config.services.dunst.package}/bin/dunst &"
            "${config.programs.waybar.package}/bin/waybar"
            "${pkgs.swaybg}/bin/swaybg --mode fill --image ${config.desktop.bgimg} &"
            "${pkgs.udiskie}/bin/udiskie &"
            "${pkgs.wl-clipboard}/bin/wl-paste --watch cliphist store"
          ] ++ (lib.lists.optionals (config.desktop.guiFramework == "gtk") [
            "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1 &"
          ]) ++ (lib.lists.optionals (config.desktop.guiFramework == "qt")
            [
            "${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1 &"
          ]);
          bind = (import ./hyprland/binds.nix) inputs;
          env = "XCURSOR_SIZE,24";
          input = {
            kb_layout = "us";
            touchpad = {
              natural_scroll = true;
            };
            sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
          };
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
            new_is_master = true;
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
        };
      };
    };
  };
  home = {
    packages = with pkgs; [
      # https://wiki.hyprland.org/Useful-Utilities/Must-have/#qt-wayland-support
      # qt5-wayland
      # qt6-wayland
      cliphist
      brightnessctl
      pavucontrol
      pulseaudio
      wdisplays
      bluez
      blueman
    ];
  };
  programs = {
    waybar = {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 30;
          spacing = 4;
          modules-left = [ "hyprland/workspaces" ];
          modules-center = [ "hyprland/window" ];
          modules-right = [
            "network"
            # "privacy"
            # "bluetooth"
            "cpu"
            "memory"
            "temperature"
            "battery"
            # "upower"
            "clock"
            "idle_inhibitor"
            "pulseaudio"
            # "mpd"
            "custom/media"
            "tray"
          ];
          "hyprland/workspaces" = {
            # disable-scroll = true;
            # all-outputs = true;
            # warp-on-scroll = false;
            # format = "{name}: {icon}";
            # format-icons = {
            #   "1" = "";
            #   "2" = "";
            # }
          };
          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
              activated = "";
              deactivated = "";
            };
          };
          cpu = {
            format = "{usage}% ";
            tooltip = false;
          };
          memory = {
            format = "{}% ";
          };
          temperature = {
            # thermal-zone = 2;
            # hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
            critical-threshold = 60;
            format-critical = "{temperatureC}°C {icon}";
            format = "{temperatureC}°C {icon}";
            format-icons = ["" "" ""];
          };
          battery = {
            states = {
              good = 95;
              warning = 20;
              critical = 5;
            };
            format = "{capacity}% {icon}";
            format-charging = "{capacity}% ";
            format-plugged = "{capacity}% ";
            format-alt = "{time} {icon}";
            format-good = "";
            format-full = "";
            format-icons = ["" "" "" "" ""];
          };
          network = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ipaddr}/{cidr} ";
            tooltip-format = "{ifname} via {gwaddr} ";
            format-linked = "{ifname} (No IP) ";
            format-disconnected = "Disconnected ⚠";
            format-alt = "{ifname}: {ipaddr}/{cidr}";
          };
          pulseaudio = {
            scroll-step = 1;
            format = "{volume}% {icon} {format_source}";
            format-bluetooth = "{volume}% {icon} {format_source}";
            format-bluetooth-muted = " {icon} {format_source}";
            format-muted = " {format_source}";
            format-source = "{volume}% ";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = ["" "" ""];
            };
            on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
          };
          clock = {
            format = "{:%a, %b %Od, %OH:%OM} ";
            calendar = {
              mode = "month";
            };
          };
        };
      };
    };
    rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      location = "center";
    };
  };
  services = {
    udiskie = {
      enable = true;
      notify = true;
      tray = "always";
    };
    dunst = {
      enable = true;
    };
  };
}
