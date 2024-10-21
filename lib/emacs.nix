{ pkgs, config, nix-doom-emacs, ... }:
let
  # dictionary = { size ? "70", spelling ? "US", diacritic ? "keep", special ? "hacker", encoding ? "utf-8", format ? "inline", download ? "aspell"}: pkgs.stdenv.mkDerivation {
  #   pname = "Dictionary";
  #   src = pkgs.fetchurl {
  #     url = "http://app.aspell.net/create?max_size=70&spelling=US&max_variant=2&diacritic=keep&special=hacker&encoding=utf-8&format=inline&download=aspell";
  #   };
  # };
  remove-files = files: drv: pkgs.runCommand "${drv.name}-removed" {} ''
    mkdir $out
    cp ${drv}/* $out
    ${builtins.concatStringsSep "\n" (builtins.map (file: "rm $out/${file}") files)}
  '';
in {
  imports = [ nix-doom-emacs.hmModule ];
  home = {
    packages = [
      (pkgs.aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
      (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
      (pkgs.writeShellScriptBin "emacsclient-pager" ''
        set -e
        mkdir --parents /tmp/emacsclient-pager
        file="$(mktemp /tmp/emacsclient-pager/XXXXX.pager)"
        cat >"''${file}" </dev/stdin &
        catpid=$!
        emacsclient "''${file}"
        if ps --pid "''${catpid}" > /dev/null; then
           kill --signal TERM --timeout 1000 KILL "''${catpid}"
        fi
        rm "''${file}"
      '')
      pkgs.fira
      pkgs.emacs-all-the-icons-fonts
      pkgs.fontconfig
      pkgs.shellcheck
      pkgs.rustc
      pkgs.cargo
      pkgs.racket
      #pkgs.pipenv
      pkgs.rust-analyzer
      # pkgs.plantuml
      pkgs.nixfmt-rfc-style
      pkgs.grip
      pkgs.html-tidy
      pkgs.stylelint
      pkgs.nodePackages_latest.js-beautify
    ];
    sessionVariables = {
      EDITOR = "emacsclient";
      PAGER = "emacsclient-pager";
      DOOMDIR = "${config.xdg.configHome}/doom";
    };
    sessionPath = [
      "${config.home.homeDirectory}/emacs.d/bin"
    ];
    file = {
      "${config.home.sessionVariables.DOOMDIR}/init.el" = {
        source = ./doom/init.el;
      };
      "${config.home.sessionVariables.DOOMDIR}/packages.el" = {
        source = ./doom/init.el;
      };
      "${config.home.sessionVariables.DOOMDIR}/config.el" = {
        source = ./doom/config.el;
      };
    };
  };
  programs = {
    emacs = {
      enable = true;
      package = pkgs.emacs30-pgtk;
      extraPackages = epkgs: [
        epkgs.vterm
        epkgs.doom
      ];
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
        exec = "${config.programs.emacs.package}/bin/emacsclient -c";
        terminal = false;
        categories = [ "Utility" "TextEditor" ];
        mimeType = [ "text/plain" ];
        icon = "${config.programs.emacs.package}/share/icons/hicolor/scalable/apps/emacs.svg";
      };
    };
  };
}
