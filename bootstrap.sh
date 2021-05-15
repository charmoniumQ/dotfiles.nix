#!/usr/bin/env bash

set -e -x
wd="$(dirname ${0})"

if [ ! -d "${HOME}/.nix-profile" ]; then
	tmp=$(mktemp)
	curl -L https://nixos.org/nix/install > "${tmp}"
	sh "${tmp}" --daemon
fi

if ! which nix > /dev/null; then
	source ${HOME}/.nix-profile/etc/profile.d/nix.sh
fi

if ! which home-manager > /dev/null; then
	nix-env -i home-manager
fi

home-manager -f "${wd}/home.nix" switch

zsh="$(which zsh)"

if [ -f /etc/shells ] && ! grep "${zsh}" /etc/shells; then
	echo "${zsh}" | sudo tee -a /etc/shells > /dev/null
fi

if [ "${SHELL}" != "${zsh}" ]; then
	if which chsh > /dev/null; then
		chsh -s "${zsh}"
	else
		echo "Could not make zsh default shell"
	fi
fi
