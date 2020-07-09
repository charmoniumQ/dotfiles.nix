# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

PROFILE=1

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
	. "${HOME}/.bashrc"
    fi
fi

export PATH="${HOME}/.local/bin:${PATH}" # add my scripts

export EDITOR=emc
export PAGER=emc

# Python/pyenv conf
export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:${PATH}"
if command -v pyenv 1>/dev/null 2>&1; then
	eval "$(pyenv init -)"
fi

# haskell conf
export PATH="${HOME}/.cabal/bin:${PATH}"

# java conf
export JAVA_HOME="/usr/lib/jvm/default-java"

# perl conf
# export PATH="${HOME}/perl5/bin:${PATH}" # add perl scripts
# export PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
# export PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
# export PERL_MB_OPT="--install_base \"${HOME}/perl5\""
# export PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"

# Node.JS conf
export NVM_DIR="${HOME}/.nvm"
[ -s "${NVM_DIR}/nvm.sh" ] && . "${NVM_DIR}/nvm.sh" # This loads nvm
#export NODE_PATH=/home/sam/.yarn/global/node_modules
export PATH="${HOME}/.yarn/bin:${PATH}"

# ld/cc/cpp conf
# export LD_LIBRARY_PATH=${HOME}/.local/lib/libcriterion.so:${HOME}/.local/lib:${LD_LIBRARY_PATH}
# export C_INCLUDE_PATH=${HOME}/.local/include:${C_INCLUDE_PATH}
# export CXX=clang++
# export CC=clang

# XDG conf
# export XDG_RUNTIME_DIR=/tmp/runtime-sam

# Ruby conf
export RBENV_VERSION=2.6.3
export PATH="${HOME}/.rbenv/bin:${HOME}/.rbenv/shims:$PATH"

# Go conf
export GOPATH="${HOME}/go"
export PATH="${GOPATH}/bin:${PATH}"

# Direnv
eval "$(direnv hook zsh)"

# fzf conf
export FZF_DEFAULT_COMMAND='fd --type f'
#export FZF_DEFAULT_OPTS="--layout=reverse --inline-info"

# SSH agent
# if [ ! -S ~/.ssh/ssh_auth_sock ]; then
#   eval `ssh-agent`
#   ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
# fi
# export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock

# editor
# TODO: this

# rust conf
export CARGO_PATH="${HOME}/.cargo"
export PATH="${CARGO_PATH}/bin:$PATH"

# GPG conf
# export GPG_TTY=$(tty)
# export GPG_KEY='BD4496A020346A68C4F7AA4F27C6B277411E28CD'

# Ruby
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# C/C++
export CXX=clang++
export CC=clang++
