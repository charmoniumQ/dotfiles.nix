{
  description = "nix-doom-emacs shell";

  inputs = {
    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-unstable;
    };
    nix-doom-emacs = {
      url = github:nix-community/nix-doom-emacs;
      inputs = {
        nix-straight = {
          url = github:codingkoi/nix-straight.el/codingkoi/apply-librephoenixs-fix;
        };
      };
    };
  };

  outputs = { self, nixpkgs, nix-doom-emacs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    doom-emacs = nix-doom-emacs.packages.${system}.default.override {
      doomPrivateDir = ./doom.d;
    };
  in
  {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [ doom-emacs ];
    };
  };
}
