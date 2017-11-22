[[ `tty` = "/dev/tty1" ]] && startx &> /var/log/xlog

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'l:|=* r:|=*' 'r:|[._-/]=** r:|=**'
zstyle :compinstall filename '/home/sam/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -e
# End of lines configured by zsh-newuser-install

autoload colors
colors

export PROMPT="%(?.$fg[green].$fg[red]%? )n%#$reset_color "
export PATH="/opt/spark-2.1.0-bin-hadoop2.7/bin/:/opt/dfu-util/src/:/opt/google-cloud-sdk/bin:/opt/scala-2.11.8/bin/:/opt/sbt/bin:/$HOME/.rbenv/bin:/home/sam/dotfiles/bin/:$PATH"
export PAGER=less
export JAVA_HOME="/usr/lib/jvm/openjdk"
export GOPATH="/home/sam/gocode"
export PATH="$GOPATH/bin:$PATH"
# export PYTHONPATH=".:$PYTHONPATH"
alias la="ls -ahlt"
alias ipy="ipython3"
alias ipy3="ipython3"
alias ipy2="ipython2"

function rm() {
	mv -f "$@" /trash
}

eval "$(rbenv init -)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f /home/sam/Downloads/google-cloud-sdk/path.zsh.inc ]; then
  source '/opt/google-cloud-sdk/path.zsh.inc'
fi

# The next line enables shell command completion for gcloud.
if [ -f /home/sam/Downloads/google-cloud-sdk/completion.zsh.inc ]; then
  source '/opt/google-cloud-sdk/completion.zsh.inc'
fi


#source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
#source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

[[ -z "$TMUX" ]] && exec tmux

PATH="/home/sam/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/sam/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/sam/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/sam/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/sam/perl5"; export PERL_MM_OPT;

# openvpn
# echo "nameserver 8.8.8.8\nnameserver 8.8.4.4\n" | sudo tee /etc/resolv.conf.head
# get login info
# sudo openvpn "/etc/openvpn/US East.ovpn"
