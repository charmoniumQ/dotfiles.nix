{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      gscan2pdf
      sane-backends
      imagemagick
    ];
  };
}
