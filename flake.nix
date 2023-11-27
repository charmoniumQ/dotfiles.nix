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
  };

  outputs = (
    { nixpkgs
    , home-manager
    , nix-doom-emacs
    , flake-utils
    , self
    }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations = {
        sam = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = {
            inherit nix-doom-emacs;
            nproc = 4;
          };
          inherit pkgs;
          modules = [
            ./user-specific-config.nix
            ./lib/cli.nix
            ./lib/devtools.nix
            ./lib/emacs.nix
            ./lib/home-manager.nix
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
                ${home-manager.packages.${system}.home-manager}/bin/home-manager \
                  --print-build-logs \
                  --keep-going \
                  --show-trace \
                  --flake . \
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
