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
fi

if ! groups $(whoami) | grep docker; then
	sudo groupadd docker
	sudo usermod -aG docker $(whoami)
fi

# snap install zoom spotify discord slack fractal telegram beeper firefox google-chrome
