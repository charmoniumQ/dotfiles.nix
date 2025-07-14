{ pkgs, config, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        pkgs.pandoc
        pkgs.texlive.combined.scheme-full
        haskellPackages.pandoc-crossref
      ];
    };
  };
}
