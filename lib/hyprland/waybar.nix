{ pkgs, ... }: {
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
      "bluetooth"
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
      disable-scroll = false;
      active-only = false;
	    format-icons = {
		    active = "";
		    default = "";
	    };
      # all-outputs = true;
      # warp-on-scroll = false;
      format = "{icon}{id}";
    };
    "hyprland/windows" = {
      rewrite = {
        "(.*) — Mozilla Firefox" = "󰈹";
        "(.*) — Doom Emacs" = " $1";
      };
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
      format-wifi = "{essid:.10} ({signalStrength}%) ";
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
      format = "{:%a %d, %OH:%OM} ";
      tooltip-format = "{:%Y %b %Od, %a, %OH:%OM:%OS} ";
      calendar = {
        mode = "month";
      };
    };
  };
}
