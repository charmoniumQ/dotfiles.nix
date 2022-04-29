# TODO: Use home-manager native way of sourcing the Nix profile env in zshenv.
# Ideally, this would work with Alt-F2 as well.
source ${HOME}/.nix-profile/etc/profile.d/nix.sh

export KEEPASSDB=${HOME}/box/Database.kdbx

export PATH="${HOME}/.local/bin:${PATH}"

# For Conda
if [ -f ~/miniconda3/etc/profile.d/conda.sh ]; then
	source ~/miniconda3/etc/profile.d/conda.sh
fi

# For Spack
#source ${HOME}/.local/opt/spack/share/spack/setup-env.sh
