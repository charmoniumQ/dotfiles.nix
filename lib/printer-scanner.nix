{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      xsane
      sane-backends
      imagemagick
    ];
  };
}
