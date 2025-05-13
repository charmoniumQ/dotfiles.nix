{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
      # System tools
      google-cloud-sdk
      asciinema

      # Containers
      #podman # Must be installed by host (NixOS or other)
      dive
      diffoci
      proot
      # TODO: fix diffoscope

      # Pictures
      mermaid-cli
      d2
      ditaa

      # JS tools
      nodejs
      yarn-berry

      # Ruby tools
      ruby
      rubocop
      # I guess bundler is included with Ruby now?
      # bundler

      # Rust tools
      # For long-term projects, you should use Crane in a Flake
      # For experimentation, it's nice to have a default version installed
      rustup

      # Data munging
      gephi
      sqlitebrowser
      miller
      pup

      # Java tools
      jdk
      maven
      gradle

      # VCS
      git-machete
      bfg-repo-cleaner
      mercurial
      subversion

      # VM tools
      vagrant

      # Nix tools
      alejandra

      # Sem Web tools
      protege-distribution
    ];
  };
}
