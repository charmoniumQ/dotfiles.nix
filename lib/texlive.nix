{ pkgs, config, flox, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        (pkgs.texlive.combine {
          inherit (texlive) scheme-small;
        })
        haskellPackages.pandoc-crossref
      ];
    };
  };
}
