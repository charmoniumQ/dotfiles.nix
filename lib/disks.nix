{ ... }: {
  services = {
    udiskie = {
      enable = true;
      notify = true;
      tray = "always";
    };
  };
}
