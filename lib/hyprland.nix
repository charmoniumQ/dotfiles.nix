{ pkgs, config, lib, ... }@inputs: {
  imports = [
    ./hyprland/hyprshade.nix
  ];
  wayland = lib.attrsets.optionalAttrs config.desktop.enable {
    windowManager = {
      hyprland = {
        enable = true;
        settings = (import ./hyprland/conf.nix) inputs;
      };
    };
  };
  home = lib.attrsets.optionalAttrs config.desktop.enable {
    packages = with pkgs; [
      # https://wiki.hyprland.org/Useful-Utilities/Must-have/#qt-wayland-support
      # qt5-wayland
      # qt6-wayland

      # Required for waybar
      font-awesome

      # required for screensharing on Wayland
      # https://gist.github.com/PowerBall253/2dea6ddf6974ba4e5d26c3139ffb7580
      # https://wiki.hyprland.org/Useful-Utilities/Hyprland-desktop-portal/
      # https://www.reddit.com/r/hyprland/comments/10xi2md/hyprland_zoom_screen_share/
      grim
      slurp
      networkmanagerapplet
      # TODO: cliphist
      # https://nixpk.gs/pr-tracker.html?pr=348887
      # cliphist
      wl-clipboard
      brightnessctl
      pavucontrol
      pulseaudio
      (wdisplays.overrideAttrs (attr: {
        patches = (if attr ? patches then attr.patches else []) ++ [(fetchurl {
          url = "https://github.com/artizirk/wdisplays/commit/50e549465d63cdcac1deb385d437a66cb2d08f43.diff";
          hash = "sha256-W9Qk4aX0s+thNy9Slsaz/v2dw+4bLV/OsWH4vhgtXRE=";
        })];
      }))
      nwg-displays
      kanshi
      bluez
      blueman
      (pkgs.writeShellScriptBin "mirror" ''
        read -p "Primary monitor: " PRIMARY_MONITOR
        read -p "Secondary monitor: " SECONDARY_MONITOR
        ${pkgs.hyprland}/bin/hyprctl keywords monitor $SECONDARY_MONITOR,preferred,auto,1,mirror,$PRIMARY_MONITOR
      '')
      (pkgs.writeShellScriptBin "homedesk" ''
        set -e -x
        laptop=eDP-1
        dell=$(hyprctl monitors -j | jq '.[] | select(.make == "Dell Inc.") | .name' --raw-output | head --lines 1)
        acer=$(hyprctl monitors -j | jq '.[] | select(.make == "Acer Technologies") | .name' --raw-output | head --lines 1)
        ${pkgs.hyprland}/bin/hyprctl keywords monitor $acer,preferred,0x0,1
        ${pkgs.hyprland}/bin/hyprctl keywords monitor $laptop,1152x768,1000x0,2
        ${pkgs.hyprland}/bin/hyprctl keywords monitor $dell,preferred,2000x0,1,transform,3
        ${pkgs.hyprland}/bin/hyprctl dispatch moveworkspacetomonitor 1 $acer
        ${pkgs.hyprland}/bin/hyprctl dispatch moveworkspacetomonitor 2 $laptop
        ${pkgs.hyprland}/bin/hyprctl dispatch moveworkspacetomonitor 3 $dell
        ${pkgs.hyprland}/bin/hyprctl dispatch workspace 3
        ${pkgs.hyprland}/bin/hyprctl dispatch workspace 2
        ${pkgs.hyprland}/bin/hyprctl dispatch workspace 1
      '')
    ];
    sessionVariables = {
      #QT_QPA_PLATFORM = "wayland";
    };
  };
  programs = lib.attrsets.optionalAttrs config.desktop.enable {
    waybar = {
      enable = true;
      settings = (import ./hyprland/waybar.nix) inputs;
    };
    rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      location = "center";
    };
  };
  services = lib.attrsets.optionalAttrs config.desktop.enable {
    dunst = {
      enable = true;
    };
    hyprshade = {
      enable = false;
      schedule = [
        {
          name = "blue-light-filter";
          startTime = "19:00:00";
          endTime = "06:00:00";
        }
      ];
      systemd = {
        enable = true;
      };
    };
  };
  xdg = lib.attrsets.optionalAttrs config.desktop.enable {
    configFile = {
      "swappy/config" = {
        text = ''
          [Default]
          save_dir=$HOME/Pictures
          save_filename_format=swappy-%Y%m%d-%H%M%S.png
        '';
      };
    };
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-hyprland
        xdg-desktop-portal-gtk
      ];
      configPackages = [
        pkgs.hyprland
      ];
      config = {
        hyprland = {
          default = [
            "hyprland"
            "gtk"
          ];
        };
      };
    };
  };
}
