{ pkgs, nproc, nur, config, ... }: {
  nix = {
    package = pkgs.nixVersions.latest;
    settings = {
      # cores = 8;
      # max-jobs = 8;
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
      # Home-manager's permittedInsecurePackages doesn't work.
      # Use NixOS or other config tool to permit insecure packages at a system-level.
      # https://github.com/nix-community/home-manager/issues/4664
      # permittedInsecurePackages = [ ... ];
    };
  };
  # > Note, this option will not apply outside your Home Manager configuration
  # > like when installing manually through nix-env. If you want to apply it both
  # > inside and outside Home Manager you can put it in a separate file
  # https://nix-community.github.io/home-manager/options.xhtml#opt-nixpkgs.config
  xdg.configFile."nixpkgs/config.nix".text = ''
    {
      allowUnfree = true;
    }
  '';
  home = {
    packages = with pkgs; [
      git # Needed to make Nix flakes work
      nix-output-monitor
      cachix
      nh
      #flox.packages.${system}.flox
      nix-info
      alejandra
      nurl
      nix-du
      nix-tree
      #nix # get latest version of Nix from nixpkgs
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
