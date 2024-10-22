# I tried doing a fully declarative Doom emacs config with [nix-doom-emacs]. But
# that module is woefully out of date, and eventually I needed a config for
# which it didn't work.
#
# [nix-doom-emacs]: https://github.com/nix-community/nix-doom-emacs
#
# Doom already uses straight.el which is declarative already (it seems).
# Therefore, we simply use home-manager to place the config files there, and let
# Doom/straight.el handle the rest.
#
# We still need to do
#
#     git clone https://github.com/doomemacs/doomemacs ~/.emacs.d
#
# I think this can't be done by Home-manager because I think the ~/.emacs.d has
# to be writable. I could consider linking in only those files which Doom
# requires? I could also consider switching to [rycee's emacs-init hm-module].
#
# [rycee's emacs-init hm-module]: https://gitlab.com/rycee/nur-expressions/-/blob/master/hm-modules/emacs-init.nix


{ pkgs, config, lib, emacs-overlay, ... }:
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
  emacs-overlay-pkgs = pkgs.extend emacs-overlay.overlays.default;
in {
  # my guess is that the overlays you define in `nixpkgs.overlays` are only
  # applied to the nixpkgs that is made available to your user on the NIX_PATH,
  # rather than the nixpkgs used by home-manager configuration.
  #
  # --ryantm
  # https://discourse.nixos.org/t/confused-why-my-home-manager-overlay-setup-isnt-working/18758/5?u=charmoniumq

  # nixpkgs = {
  #   overlays = [ emacs-overlay.overlays.emacs ];
  # };

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
      "${config.home.homeDirectory}/.emacs.d/bin"
    ];
    file = {
      "${config.home.sessionVariables.DOOMDIR}/init.el" = {
        source = ./doom/init.el;
      };
      "${config.home.sessionVariables.DOOMDIR}/packages.el" = {
        source = ./doom/packages.el;
      };
      "${config.home.sessionVariables.DOOMDIR}/config.el" = {
        source = ./doom/config.el;
      };
    };
  };
  programs = {
    emacs = {
      enable = true;
      package = ((emacs-overlay-pkgs.emacsPackagesFor emacs-overlay-pkgs.emacs-pgtk).emacsWithPackages (
        epkgs: [ epkgs.vterm ]
      ));
      # extraPackages = epkgs: [ epkgs.emacs-vterm ];
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
