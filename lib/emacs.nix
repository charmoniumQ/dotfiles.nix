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
      EDITOR = "emacsclient";
      PAGER = "emacsclient-pager";
    };
  };
  programs = {
    doom-emacs = {
      enable = true;
      # emacsPackage = pkgs.emacs29-pgtk;
      doomPrivateDir = pkgs.buildEnv {
        name = "env";
        paths =
          let
            org-linker = pkgs.fetchFromGitHub {
              owner = "toshism";
              repo = "org-linker";
              rev = "master";
              hash = "sha256-tOvpQPH8PPdEJeN87paz3MiHrVZqOaRghNHJXm0k4yg=";
            };
            org-linker-edna = pkgs.fetchFromGitHub {
              owner = "toshism";
              repo = "org-linker-edna";
              rev = "master";
              hash = "sha256-mq9hN9I3ts/6jGwvBiOd0caizY3Mj86I7N15LyhSIkU=";
            };
            remove-files = drv: files: pkgs.runCommand "${drv.name}-removed" {} ''
              mkdir $out
              cp ${drv}/* $out
              ${builtins.concatStringsSep "\n" (builtins.map (file: "rm $out/${file}") files)}
            '';
          in [
            ./emacs
            (remove-files org-linker [ "LICENSE" "README.org" ])
            (remove-files org-linker-edna [ "LICENSE" "README.org" ])
          ];
      };
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
