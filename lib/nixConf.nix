{ pkgs, ... }: {
  nix = {
    package = pkgs.nixUnstable;
    settings = {
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      experimental-features = [ "nix-command" "flakes" ];
    };
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };
  home = {
    packages = [
      pkgs.git # Needed to make Nix flakes work
    ];
  };
}
