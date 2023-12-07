{
  description = "Home Manager configuration of sam";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
    };
    nur = {
      url = github:nix-community/NUR;
    };
  };

  outputs = (
    { nixpkgs
    , home-manager
    , nix-doom-emacs
    , flake-utils
    , nur
    , self
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations = {
        sam = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = inputs // {
            nproc = 4;
          };
          inherit pkgs;
          modules = [
            ./user-specific-config.nix
            ./lib/cli.nix
            ./lib/desktop.nix
            ./lib/devtools.nix
            ./lib/emacs.nix
            ./lib/firefox.nix
            ./lib/home-manager.nix
            ./lib/hyprland.nix
            ./lib/keyring.nix
            ./lib/nextcloud.nix
            ./lib/nixConf.nix
            ./lib/xdg-ninja.nix
            ./lib/xonsh.nix
            ./lib/zsh.nix
          ];
        };
      };
      apps = {
        "${system}" = {
          home-manager = {
            program = "${home-manager.packages.${system}.home-manager}/bin/home-manager";
            type = "app";
          };
          switch = {
            program = let
              switch-package = pkgs.writeShellScriptBin "script" ''
                f=/home/sam/.mozilla/firefox/default/search.json.mozlz4
                if [ -f $f ]; then
                  mv $f $f.backup
                fi
                ${home-manager.packages.${system}.home-manager}/bin/home-manager \
                  --print-build-logs \
                  --keep-going \
                  --show-trace \
                  --flake . \
                  -b backup \
                  switch
              '';
            in "${switch-package}/bin/script";
            type = "app";
          };
        };
      };
      devShells = {
        "${system}" = {
          default = pkgs.mkShell {
            packages = [ home-manager.packages.${system}.default ];
          };
        };
      };
    }
  );
}
