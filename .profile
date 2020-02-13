export PATH="/usr/local/bin:/usr/bin:/bin:/usr/games"
export PATH="$HOME/.local/scripts:$HOME/.local/bin:${PATH}" # add my scripts

# python conf
export PATH="$HOME/.local/venv/bin:${PATH}" # add python scripts

# haskell conf
export PATH="$HOME/.cabal/bin:${PATH}"

# java conf
export JAVA_HOME="/usr/lib/jvm/default-java"

# perl conf
export PATH="$HOME/perl5/bin:${PATH}" # add perl scripts
export PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"

# JS conf
export NODE_PATH=/home/sam/.config/yarn/global/node_modules
export PATH="$HOME/.yarn/bin:${PATH}"

# ld/cc/cpp conf
# export LD_LIBRARY_PATH=$HOME/.local/lib/libcriterion.so:$HOME/.local/lib:${LD_LIBRARY_PATH}
# export C_INCLUDE_PATH=$HOME/.local/include:${C_INCLUDE_PATH}
export CXX=clang++
export CC=clang

# XDG conf
export XDG_RUNTIME_DIR=/tmp/runtime-sam

# Ruby conf
export RBENV_VERSION=2.6.3
export PATH="$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH"

# Go conf
export GOPATH="${HOME}/go"
export PATH="${GOPATH}/bin:${PATH}"

# fzf conf
export FZF_DEFAULT_COMMAND='fd --type f'
#export FZF_DEFAULT_OPTS="--layout=reverse --inline-info"

# SSH agent
if [ ! -S ~/.ssh/ssh_auth_sock ]; then
  eval `ssh-agent`
  ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock

# if [ -n "$DESKTOP_SESSION" ];then
#    #eval $(gnome-keyring-daemon --start)
#    export SSH_AUTH_SOCK
# fi

# # https://stackoverflow.com/a/18915067/1078199
# SSH_ENV="$HOME/.ssh/environment"

# function start_agent {
#     echo "Initialising new SSH agent..."
#     /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
#     chmod 600 "${SSH_ENV}"
#     . "${SSH_ENV}" > /dev/null
# }

# # Source SSH settings, if applicable

# if [ -f "${SSH_ENV}" ]; then
#     . "${SSH_ENV}" > /dev/null
#     #ps ${SSH_AGENT_PID} doesn't work under cywgin
#     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
#         start_agent;
#     }
# else
#     start_agent;
# fi

# editor
if which emc > /dev/null
then
	export EDITOR=emc
	#export PAGER=emc-pager
else
	export EDITOR=nano
fi

export VISUAL=$EDITOR

# rust conf
export PATH="$HOME/.cargo/bin:$PATH"

# GPG conf
export GPG_TTY=$(tty)
export GPG_KEY='BD4496A020346A68C4F7AA4F27C6B277411E28CD'
