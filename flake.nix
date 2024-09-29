{
  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs = {
      url = github:nixos/nixpkgs/nixpkgs-unstable;
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
    nix-doom-emacs = {
      url = github:nix-community/nix-doom-emacs;
      inputs = {
        nix-straight = {
          url = github:codingkoi/nix-straight.el/codingkoi/apply-librephoenixs-fix;
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
    , self
    , nix-index-database
    , ...
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations = {
        laptop = home-manager.lib.homeManagerConfiguration {
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
                profile=''${1:-laptop}
                shift
                echo $@
                ${home-manager.packages.${system}.home-manager}/bin/home-manager \
                  --print-build-logs \
                  --keep-going \
                  --show-trace \
                  --verbose \
                  --flake .#''${profile} \
                  -b backup \
                  switch $@
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
