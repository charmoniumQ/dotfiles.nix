{ pkgs, config, lib, ... }@inputs: {
  wayland = {
    windowManager = {
      hyprland = {
        enable = true;
        settings = (import ./hyprland/conf.nix) inputs;
      };
    };
  };
  home = {
    packages = with pkgs; [
      # https://wiki.hyprland.org/Useful-Utilities/Must-have/#qt-wayland-support
      # qt5-wayland
      # qt6-wayland
      cliphist
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
      bluez
      blueman
    ];
  };
  programs = {
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
