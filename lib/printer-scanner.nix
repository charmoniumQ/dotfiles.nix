{ pkgs, ... }: {
  home = {
    pacakges = with pkgs; [
      xsane
      sane-backends
      imagemagick
    ];
  };
}
