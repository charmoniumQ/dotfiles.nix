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
export PATH="/$HOME/.rbenv/bin:/home/sam/dotfiles/bin/:$PATH"
export PAGER=less
alias la="ls -ahlt"

eval "$(rbenv init -)"

#source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
#source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

[[ -z "$TMUX" ]] && exec tmux
