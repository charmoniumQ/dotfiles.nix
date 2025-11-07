{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      bluez
      blueman
      bluetuith
    ];
  };
}
