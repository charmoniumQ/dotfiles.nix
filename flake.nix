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
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    nur = {
      url = github:nix-community/NUR;
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    nix-index-database = {
      url = github:Mic92/nix-index-database;
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    # flox = {
    #   url = github:flox/floxpkgs;
    #   inputs = {
    #     nixpkgs = {
    #       follows = "nixpkgs";
    #     };
    #   };
    # };
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
      mkConfig = profile: extraConf: home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = inputs // {
            nproc = 8;
            system = system;
          };
          inherit pkgs;
          modules = [
            profile
            extraConf
          ];
        };
    in {
      homeConfigurations = {
        "sam@laptop" = mkConfig ./profiles/laptop.nix {};
        remote = mkConfig ./profiles/remote.nix {};
        "sagrays@s1087928" = mkConfig ./profiles/other.nix {desktop = { enable = true; }; };
        "sysadmin@home-server" = mkConfig ./profiles/tv.nix {};
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
                  . \
                  -- \
                  --keep-going \
                   $@
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
