#!/usr/bin/env sh

set -x -e

echo "# Modifications here will not be kept" | sudo tee /etc/apt/sources.list

cat <<EOF | sudo tee /etc/apt/sources.list.d/ubuntu.list
deb http://archive.ubuntu.com/ubuntu/ focal main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu/ focal-updates main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu/ focal-security main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu/ focal-backports main restricted universe multiverse
deb http://archive.canonical.com/ubuntu focal partner
EOF

wget -qO - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' | sudo tee /etc/apt/sources.list.d/google-chrome.list

echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" | sudo tee /etc/apt/sources.list.d/google-cloud-sdk.list
wget -qO- https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -

echo "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list
wget -qO- https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

# sudo apt update && sudo apt install -y \
# 	firefox \
# 	adobe-flashplugin browser-plugin-freshplayer-pepperflash \
# 	google-chrome-beta \
# 	build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl git \
# 	thunderbird \
# 	zsh \
# 	emacs \
#	meld \
# 	git \
# 	htop \
# 	golang-go \
# 	docker-ce docker-ce-cli containerd.io \
# 	texinfo \
# ;

# sudo snap install --classic \
# 	slack \
# ;

if [ $SHELL != "$(which zsh)" ]; then
	chsh -s $(which zsh) ${USER}
fi

git_clone_if_not_exists() {
	repo="${1}"
	dest="${2}"
	if [ ! -d "${dest}" ]; then
		git clone "${repo}" "${dest}"
	fi
	git -C "${dest}" pull origin master
}

git_clone_if_not_exists https://github.com/pyenv/pyenv.git ${HOME}/.pyenv
export PATH="${HOME}/.pyenv/bin:${PATH}"
eval "$(pyenv init -)"
latest=$(pyenv install -l | tr -d ' ' | grep -v dev | grep -P '^\d+\.\d+\.\d+$' | tail -n 1)
set +x
pyenv install -s "${latest}"
set -x
pyenv global "${latest}"
pip3 install --upgrade \
	pip \
	poetry \
	'python-language-server[all]' \
	trash-cli \
	ipython \
	jupyter \
	numpy \
	scipy \
	matplotlib \
	pandas \
	passwdgen \
;

git_clone_if_not_exists https://github.com/ohmyzsh/ohmyzsh.git ${HOME}/.oh-my-zsh
git_clone_if_not_exists https://github.com/junegunn/fzf.git ${HOME}/.oh-my-zsh/custom/plugins/fzf --depth 1
git_clone_if_not_exists https://github.com/zsh-users/zsh-autosuggestions ${HOME}/.oh-my-zsh/custom/plugins/zsh-autosuggestions
git_clone_if_not_exists https://github.com/zsh-users/zsh-syntax-highlighting ${HOME}/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting

git_clone_if_not_exists https://github.com/nvm-sh/nvm.git "${HOME}/.nvm"
set +x
. "${HOME}/.nvm/nvm.sh"
nvm install node
set -x
npm install -g yarn

go get \
	github.com/direnv/direnv \
	github.com/junegunn/fzf \
;

if ! which cargo; then
	curl https://sh.rustup.rs -sSf | sh
	. ~/.profile
fi
cargo install \
	exa \
	ripgrep \
	fd-find \
;

curl -sSL https://get.rvm.io | bash

git_clone_if_not_exists https://github.com/TheLocehiliosan/yadm.git ${HOME}/.yadm-project
rm -f ${HOME}/.local/bin/yadm
mkdir -p ${HOME}/.local/bin
ln -s ${HOME}/.yadm-project/yadm ${HOME}/.local/bin/yadm

mkdir -p ~/.config/tmux
git_clone_if_not_exists https://github.com/tmux-plugins/tpm ~/.config/tmux/tpm

mkdir -p ${HOME}/.local
wget -qO ${HOME}/.local/background.jpg https://upload.wikimedia.org/wikipedia/commons/a/a8/Nighthawks_by_Edward_Hopper_1942.jpg
