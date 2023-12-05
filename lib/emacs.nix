{ pkgs, config, nix-doom-emacs, ... }:
let
  # dictionary = { size ? "70", spelling ? "US", diacritic ? "keep", special ? "hacker", encoding ? "utf-8", format ? "inline", download ? "aspell"}: pkgs.stdenv.mkDerivation {
  #   pname = "Dictionary";
  #   src = pkgs.fetchurl {
  #     url = "http://app.aspell.net/create?max_size=70&spelling=US&max_variant=2&diacritic=keep&special=hacker&encoding=utf-8&format=inline&download=aspell";
  #   };
  # };
in {
  imports = [ nix-doom-emacs.hmModule ];
  home = {
    packages = [
      (pkgs.aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
      (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
      pkgs.nodePackages.vscode-langservers-extracted
      pkgs.nodePackages.unified-language-server
      # dictionary
    ];
    sessionVariables = {
      # Make sure GUI Emacs shows up in launcher
      XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
      # TODO: is the above line really necessary?
      EDITOR = "emacsclient -c";
    };
  };
  programs = {
    doom-emacs = {
      enable = true;
      # emacsPackage = pkgs.emacs29-pgtk;
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
        exec = "${config.programs.doom-emacs.package}/bin/emacsclient -c";
        terminal = false;
        categories = [ "Utility" "TextEditor" ];
        mimeType = [ "text/plain" ];
        icon = "${config.programs.doom-emacs.package}/share/icons/hicolor/scalable/apps/emacs.svg";
      };
    };
  };
}
