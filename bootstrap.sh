#!/usr/bin/env bash

set -e -x
wd="$(dirname ${0})"

if ! which nix; then
	tmp=$(mktemp)
	curl -L https://nixos.org/nix/install > "${tmp}"
	sh "${tmp}" --daemon
	source /etc/profile.d/nix.sh
	nix-env -i hello
fi

source /etc/profile.d/nix.sh
# source ${HOME}/.nix-profile/etc/profile.d/nix.sh

if ! which home-manager > /dev/null; then
	nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
	nix-channel --update
	export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
	nix-shell '<home-manager>' -A install
fi

home-manager -f "${wd}/home.nix" switch

zsh="$(which zsh)"

if [ -f /etc/shells ] && ! grep "${zsh}" /etc/shells; then
	echo "${zsh}" | sudo tee -a /etc/shells > /dev/null
fi

if [ "${SHELL}" != "${zsh}" ]; then
	if which chsh > /dev/null; then
		sudo chsh -s "${zsh}"
	else
		echo "Could not make zsh default shell"
	fi
fi


if ! docker run hello-world > /dev/null; then
	if ! which curl > /dev/null; then
		sudo apt-get install \
			 apt-transport-https \
			 ca-certificates \
			 curl \
			 gnupg \
			 lsb-release
	fi

	if [ ! -f /usr/share/keyrings/docker-archive-keyring.gpg ]; then
		curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
	fi

	if [ ! -f /etc/apt/sources.list.d/docker.list ]; then
		echo \
			"deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" \
			| sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
		sudo apt-get update
	fi

	sudo apt-get install docker-ce docker-ce-cli containerd.io

	if ! groups $(whoami) | grep docker; then
		sudo groupadd docker
		sudo usermod -aG docker $(whoami)
	fi
fi
