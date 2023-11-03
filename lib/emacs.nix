{ pkgs, config, nix-doom-emacs, ... }: {
  imports = [ nix-doom-emacs.hmModule ];
  home = {
    packages = [
      (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];
  };
  programs = {
    doom-emacs = {
      enable = true;
      doomPrivateDir = ./emacs;
      doomPackageDir = pkgs.stdenv.mkDerivation {
        name = "doom-without-config";
        src = builtins.path {
          path = ./emacs;
          name = "doom-private-dir-filtered";
          filter = path: type:
            builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
        };
        installPhase = ''
          mkdir $out
          cp init.el $out
          cp packages.el $out
          touch $out/config.el
        '';
      };
    };
  };
  services = {
    emacs = {
      enable = true;
    };
  };
  fonts = {
    fontconfig = {
      enable = true;
    };
  };
  xdg = {
    desktopEntries = {
      emacs = {
        name = "Doom Emacs";
        genericName = "Text Editor";
        exec = "${config.programs.doom-emacs.package}/bin/emacsclient %U";
        terminal = false;
        categories = [ "Utility" "TextEditor" ];
        mimeType = [ "text/plain" ];
        icon = "${config.programs.doom-emacs.package}/share/icons/hicolor/scalable/apps/emacs.svg";
      };
    };
  };
}
