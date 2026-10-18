{
  mainBar = {
    height = 5;
    reload_style_on_change = true;
    modules-left = [
      "hyprland/workspaces"
      "sway/mode"
    ];
    modules-center = [
      "hyprland/window"
    ];
    modules-right = [
      "pulseaudio"
      "idle_inhibitor"
      "network"
      "power-profiles-daemon"
      "cpu"
      "memory"
      "temperature"
      "battery"
      "clock"
      "tray"
    ];
    idle_inhibitor = {
      format = "{icon}";
      format-icons = {
        activated = "";
        deactivated = "";
      };
    };
    clock = {
      # timezone = "America/New_York";
      format = "{:%H:%M %b %d (%U)}";
      tooltip-format = "<tt><small>{calendar}</small></tt>";
    };
    cpu = {
      format = "{usage}% ";
    };
    memory = {
      format = "{}% ";
    };
    temperature = {
      # thermal-zone = 2;
      # hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
      critical-threshold = 80;
      format-critical = "{temperatureC}°C {icon}";
      format = "{temperatureC}°C {icon}";
      format-icons = [""];
    };
    backlight = {
      format = "{percent}% {icon}";
      format-icons = ["" "" "" "" "" "" "" "" ""];
      min-brightness = 1;
    };
    battery = {
      states = {
        critical = 15;
      };
      format = "{capacity}% {icon}";
      format-full = "{capacity}% {icon}";
      format-charging = "{capacity}% 󰃨";
      format-plugged = "{capacity}% ";
      format-alt = "{time} {icon}";
      # format-good "" // An empty format will hide the module
      # format-full = ""
      format-icons = ["" "" "" "" ""];
    };
    power-profiles-daemon = {
      format = "{icon}";
      tooltip-format = "Power profile: {profile}\nCPU driver: {cpu_driver}\nPlatform driver: {platform_driver}";
      tooltip = true;
      format-icons = {
        default = "";
        performance = "";
        balanced = "";
        power-saver = "";
      };
    };
    network = {
      # "interface": "wlp2*" // (Optional) To force the use of this interface
      format-wifi = "";
      format-ethernet = "📠";
      format-disconnected = "";
      tooltip-format = "{ifname} via {gwaddr}\n{ipaddr}/{cidr}\n{essid} ({signalStrength}%)";
    };
    pulseaudio = {
      # "scroll-step": 1 // % can be a float
      format = "{volume}% {icon} {format_source}";
      format-bluetooth = "{volume}% {icon}<sup></sup> {format_source}";
      format-bluetooth-muted = "🔇 {icon}<sup></sup> {format_source}";
      format-muted = "🔇 {format_source}";
      format-source = "{volume}% ";
      format-source-muted = "";
      format-icons = {
        headphone = "";
        hands-free = "";
        headset = "";
        phone = "";
        portable = "";
        car = "";
        default = ["" "" ""];
      };
      on-click = "pavucontrol";
    };
  };
}
