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
      wdisplays
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
