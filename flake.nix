{
  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-unstable;
    };
    home-manager = {
      url = github:nix-community/home-manager;
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    flake-utils = {
      url = github:numtide/flake-utils;
    };
    emacs-overlay = {
      url = github:nix-community/emacs-overlay;
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    nur = {
      url = github:nix-community/NUR;
    };
    nix-index-database = {
      url = github:Mic92/nix-index-database;
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    flox = {
      url = github:flox/floxpkgs;
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs = (
    { nixpkgs
    , home-manager
    , emacs-overlay
    , ...
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations = {
        "sam@laptop" = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = inputs // {
            nproc = 8;
            system = system;
          };
          inherit pkgs;
          modules = [
            ./profiles/laptop.nix
          ];
        };
        remote = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = inputs // {
            nproc = 8;
            system = system;
          };
          inherit pkgs;
          modules = [
            ./profiles/remote.nix
          ];
        };
      };
      apps = {
        "${system}" = {
          home-manager = {
            program = "${home-manager.packages.${system}.home-manager}/bin/home-manager";
            type = "app";
          };
          apply = {
            program = let
              switch-package = pkgs.writeShellScriptBin "script" ''
                set -ex
                files=(
                  "$HOME/.mozilla/firefox/default/search.json.mozlz4"
                  "$HOME/.config/mimeapps.list"
                )
                for file in "''${files[@]}"; do
                  if [ -f "$file" ]; then
                    mv "$file" "$file.backup"
                  fi
                done
                ${pkgs.nh}/bin/nh home switch \
                  --backup-extension backup \
                  . $@
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
