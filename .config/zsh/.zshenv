# TODO: Use home-manager native way of sourcing the Nix profile env in zshenv.
# Ideally, this would work with Alt-F2 as well.
source ${HOME}/.nix-profile/etc/profile.d/nix.sh

export PATH="${HOME}/.local/bin:${PATH}"
