{ pkgs, nproc, nur, ... }: {
  nix = {
    package = pkgs.nixVersions.latest;
    settings = {
      cores = 2;
      max-jobs = nproc / 2;
      use-xdg-base-directories = true;
      warn-dirty = false;
      extra-substituters = [
        "https://cache.nixos.org/"
        # "https://nix-community.cachix.org"
        # "https://ai.cachix.org"
      ];
      extra-trusted-substituters = [
        "https://cache.nixos.org/"
        # "https://nix-community.cachix.org"
        # "https://ai.cachix.org"
      ];
      extra-trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        # "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        # "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc="
      ];
      experimental-features = [ "nix-command" "flakes" ];
    };
  };
  nixpkgs = {
    overlays = [ nur.overlays.default ];
    config = {
      allowUnfree = true;
    };
  };
  home = {
    packages = with pkgs; [
      git # Needed to make Nix flakes work
      nix-output-monitor
      nh
      #flox.packages.${system}.flox
      nix-info
      alejandra
      nurl
      nix-du
    ];
    shellAliases = {
      nix-locat = "nix-locate --top-level --regex";
      nix-prefetch-url = "echo 'try nurl'";
      nix-build = "nom-build";
      nix-shell = "nom-shell";
    };
  };
  programs = {
    nix-index = {
      enable = true;
    };
    nix-index-database = {
      comma = {
        enable = true;
      };
    };
  };
}
