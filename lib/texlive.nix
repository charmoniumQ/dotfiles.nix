{ pkgs, config, flox, ... }: {
  config = {
    home = {
      packages = with pkgs; [
        (pkgs.texlive.combine {
          inherit (texlive) scheme-medium;
        })
        haskellPackages.pandoc-crossref
      ];
    };
  };
}
